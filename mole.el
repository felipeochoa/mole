;;; mole.el --- Packrat parser generator  -*- lexical-binding: t -*-

;; Copyright (C) 2018 Felipe Ochoa

;; Author: Felipe Ochoa <felipe@fov.space>
;; URL: https://github.com/felipeochoa/mole/
;; Created: 10 Nov 2017
;; Package-Requires: ((emacs "25.2"))
;; Version: 0.1
;; Keywords: maint

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;; This file is not part of GNU Emacs.

;;; Commentary:

;;; Code:

(require 'cl-lib)
(require 'subr-x)

;; this-file is inlined from 'f package
(let* ((this-file (cond
                   (load-in-progress load-file-name)
                   ((and (boundp 'byte-compile-current-file) byte-compile-current-file)
                    byte-compile-current-file)
                   (:else (buffer-file-name))))
       (default-directory (file-name-directory this-file)))
  (require 'mole-cache (expand-file-name "mole-cache"))
  (require 'mole-context (expand-file-name "mole-context")))

(declare-function mole-cache "mole-cache")
(declare-function mole-context-compare "mole-context")

(defvar mole-default-whitespace-terminal
  '(whitespace :lexical t :fuse t (* (char " \t\n\f")))
  "If a grammar doesn't specify whitespace, this value will be used.")

(defvar mole-production-keys '(:lexical :fuse :params :pass-thru)
  "List of keys that may be given in a production definition.
:LEXICAL if nil, has productions chomp whitespace and comments
before attempting a match and after a successful match.  If t, no
such chomping will be performed.

:FUSE if non-nil all children will be merged into a single
node.  Productions using :FUSE cannot call other productions.

:PARAMS if non-nil should be an arglist suitable for a lambda
form.  Applications of this production must specify productions
to bind to this arglist, which can be referenced inside the rule
body.

:PASS-THRU if non-nil, whenever the production has a single
non-empty child, no wrapper node will be created for the
production and that non-empty child will be returned instead.")

(defvar mole-default-props '()
  "Plist of `mole-production-keys' to use as defaults values.")

(defvar mole-operator-names '(: or * + \? \?= \?! opt = ! char lexical extern repetition
                                with-context if-context)
  "List of symbols reserved for mole operators.")

(defvar mole-runtime-highwater-mark 0
  "Stores the index of the last character contributing to a parse.
This position may be beyond than the end of the realized node's
contents e.g., if the character forced backtracking.")

(defvar mole-runtime-context nil
  "Alist of dynamically active context values.
A parsing context is a mapping of symbol and number keys to
arbitrary values.  The parse cache only stores one entry per
\(production . position\) pair, and it stores the parse context
alongside it.  Subsequent parsings will only use the cache if the
parsing context is shallow equal to the cached context.  If the
new context does not match the old context, then the production
is parsed again and the new result and context replace the cache
contents.")

(defvar mole-build-lexical nil
  "If t, literal parsing builders won't include whitespace chomping code.")

(defvar mole-build-fusing nil
  "Build-time dynamic variable to generate fusing nodes.")

(defvar mole-runtime-string-parse nil
  "String passed to `mole-parse-string'; used to convert to sexps later.")

(defvar mole-runtime-cache nil
  "Runtime variable holding the parse cache.")

(defvar mole-build-prod-nums nil
  "Build-time hashtable mapping production names to their numeric codes.")

(defvar mole-build-params nil
  "List containing the parameters of the production that is being built.")

(defvar mole-build-with-debug nil
  "If t, emit debugging helpers.")

(defvar mole-debug-call-stack nil
  "Runtime stack containing the named productions called.")

(defmacro mole-debug (&rest forms)
  "If `mole-build-with-debug' emit FORMS, otherwise emit nil."
  (declare (debug (&rest form)) (indent defun))
  (when mole-build-with-debug `(progn ,@forms)))

(cl-defstruct mole-grammar productions)

(cl-defstruct mole-node name children pos end)

(cl-defstruct (mole-node-operator (:include mole-node))
  "Node class for *, +, etc.")

(cl-defstruct (mole-node-literal (:include mole-node))
  "Node class for anonymous literals.") ;; just ignore name and children

(defsubst mole-node-non-empty (node)
  "Return nil if NODE matched an empty string."
  (< (mole-node-pos node) (mole-node-end node)))

(defsubst mole-node-clean-name (node)
  "Return NODE's name after unmunging."
  (mole-unmunge-production-name (mole-node-name node)))

(defmacro mole-node (name children fuse &optional pos end)
  "Construct a `mole-node' instance named NAME with CHILDREN.
If FUSE is t (evaluated at compile-time) and NAME indicates an
operator, ignore NAME and CHILDREN and return a single literal
instead.  If NAME instead indicates a custom production, its
children are instead merged into one anonymous literal.

POS and END refer to the buffer locations where the node match
started and ended.  If not given, POS defaults to the POS of the
first child and END defaults to the END of the last child.

If NAME indicates that the node should be an operator, a
`mole-node-operator' is created instead."
  (declare (debug (("quote" symbolp) form form &optional form form)))
  (cl-assert (and (consp name) (eq 'quote (car name))
                  (symbolp (setq name (cadr name)))))
  (setq fuse (eval fuse))
  (let ((kids (make-symbol "kids")))
    (unless pos (setq pos `(if ,kids (mole-node-pos (car ,kids)) (point))))
    (unless end (setq end `(if ,kids (mole-node-end (car (last ,kids))) (point))))
    `(let ((,kids ,children))
      ,(cond
        ((and (memq name mole-operator-names) fuse)
         `(mole-node-literal ,pos ,end))
        ((memq name mole-operator-names)
         `(make-mole-node-operator :name ',name :children ,kids :pos ,pos :end ,end))
        (fuse `(make-mole-node :name ',name :pos ,pos :end ,end
                               :children (list (mole-node-literal ,pos ,end))))
        (t `(make-mole-node :name ',name :children ,kids :pos ,pos :end ,end))))))

(defsubst mole-node-literal (pos end)
  "Construct a `mole-node-literal' instance.
POS and END refer to the buffer locations where the node match
started and ended."
  (make-mole-node-literal :pos pos :end end))

(cl-defmethod mole-node-to-sexp ((node mole-node))
  "Convert NODE into a test-friendly sexp."
  (cons (mole-unmunge-production-name (mole-node-name node))
        (cl-mapcan (lambda (child)
                     (if (mole-node-operator-p child)
                         (mole-node-to-sexp child)
                       (list (mole-node-to-sexp child))))
                   (mole-node-children node))))

(cl-defmethod mole-node-to-sexp ((literal mole-node-literal))
  "Convert LITERAL into a test-friendly sexp."
  (let ((beg (mole-node-literal-pos literal))
        (end (mole-node-literal-end literal)))
    (if mole-runtime-string-parse
        (substring mole-runtime-string-parse (1- beg) (1- end)) ; strings are 0-based
      (buffer-substring-no-properties beg end))))

(cl-defmethod mole-node-to-sexp ((_ (eql fail)))
  "Return 'fail if the parse failed."
  'fail)

(cl-defmethod mole-node-to-sexp ((op mole-node-operator))
  "Convert OP into a test-friendly sexp."
  (cl-mapcan (lambda (child)
               (if (mole-node-operator-p child)
                   (mole-node-to-sexp child)
                 (list (mole-node-to-sexp child))))
             (mole-node-children op)))

(cl-defmethod mole-visit ((node mole-node) fn)
  "Walk NODE's descendants and call FN on them.
FN is called twice with two arguments for each node. The first
time it's called with (DESCENDANT nil), before that node's
descendants are visited. The second time FN is called
with (DESCENDANT t), after that node's descendants are visited."
  (funcall fn node nil)
  (dolist (child (mole-node-children node))
    (mole-visit child fn))
  (funcall fn node t))

(cl-defmethod mole-visit ((node mole-node-operator) fn)
  "Visit NODE and NODE's descendants."
  (dolist (child (mole-node-children node))
    (mole-visit child fn)))

(cl-defmethod mole-visit ((_ mole-node-literal) _fn) "Noop.")
(cl-defmethod mole-visit ((_ (eql fail)) _fn) "Noop.")

(defun mole-parse-success-p (result)
  "Return t if RESULT indicates a successful parse."
  (not (eq result 'fail)))

(cl-defmacro mole-parse-match ((res-sym form) on-success &optional on-fail)
  "Execute FORM and conditionally execute a followup action.
RES-SYM is bound to the result of FORM.  If FORM is a successful
parse, execute ON-SUCCESS.  Otherwise execute ON-FAIL. ON-FAIL
defaults to simply returning 'fail."
  (declare (debug ((symbolp form) form &optional form)) (indent 1))
  `(let ((,res-sym ,form))
     (if (mole-parse-success-p ,res-sym)
         ,on-success
       ,on-fail)))

(defsubst mole-update-highwater-mark (val)
  "If VAL is greater than `mole-highwater-mark' update that value."
  (cl-callf max mole-runtime-highwater-mark val))

(defmacro mole-maybe-save-excursion (&rest body)
  "Execute BODY and restore point unless return value is non-nil."
  (declare (debug (&rest form)) (indent defun))
  (let ((point (make-symbol "point")) (res (make-symbol "res")))
    `(let ((,point (point))
           (,res (progn ,@body)))
       (unless (mole-parse-success-p ,res)
         (goto-char ,point))
       ,res)))

(defmacro mole-with-fresh-highwater-mark (&rest body)
  "Execute BODY while temporarily resetting `mole-highwater-mark'."
  (declare (indent defun) (debug (&rest form)))
  (let ((old-hwmark (make-symbol "old-hwmark"))
        (res (make-symbol "res")))
    `(let ((,old-hwmark mole-runtime-highwater-mark) ,res)
       (setq mole-runtime-highwater-mark (1- (point)))
       (setq ,res (progn ,@body))
       (mole-update-highwater-mark ,old-hwmark)
       ,res)))

(defmacro mole-ignore-hw-mark (&rest body)
  "Ignore any update to `mole-highwater-mark' in BODY."
  (declare (indent defun) (debug (&rest form)))
  `(let ((mole-runtime-highwater-mark 0))
     ,@body))

(defmacro mole-cached-result (prod-num &rest body)
  "Check the parse cache for PROD-NUM or evaluate BODY and cache it."
  (declare (debug (numberp &rest form)) (indent 1))
  (cl-assert (numberp prod-num))
  (let ((res (make-symbol "res")) (beg (make-symbol "beg")))
    `(if-let (,res (mole-cache-get mole-runtime-cache (point) ,prod-num mole-runtime-context))
         (progn (when (mole-parse-success-p (car ,res))
                  (goto-char (mole-node-end (car ,res))))
                (mole-update-highwater-mark (cdr ,res))
                (car ,res))
       (mole-with-fresh-highwater-mark
         (let ((,beg (point)))
           (setq ,res ,@body)
           (mole-cache-set mole-runtime-cache ,beg mole-runtime-highwater-mark
                           ,prod-num mole-runtime-context ,res))))))

(defmacro mole-chomp-whitespace ()
  "Chomp whitespace."
  `(mole-ignore-hw-mark (funcall mole~whitespace)))

(defmacro mole-parse-anonymous-literal (string)
  "Return a literal-parsing form for STRING."
  (declare (indent defun) (debug (stringp)))
  (if (= 1 (length string))
      `(if (eq (char-after) ,(aref string 0))
           (prog1 (mole-node-literal (point) (1+ (point)))
             (mole-update-highwater-mark (point))
             (forward-char))
         (mole-update-highwater-mark (point))
         'fail)
    (let ((i (make-symbol "i")) (pos (make-symbol "pos")))
      `(mole-maybe-save-excursion
         (let ((,i 0) (,pos (point)))
           (while (and (< ,i ,(length string)) (eq (char-after) (aref ,string ,i)))
             (forward-char)
             (cl-incf ,i))
           (if (= ,i ,(length string))
               (progn
                 (mole-update-highwater-mark (1- (point)))
                 (mole-node-literal ,pos (point)))
             (mole-update-highwater-mark (point))
             'fail))))))

(defun mole-split-spec-args (spec)
  "Split out config keys from SPEC."
  (setq spec (append spec nil))
  (let ((config spec) tail)
    (while (memq (car spec) mole-production-keys)
      (setq tail (cdr spec)
            spec (cddr spec)))
    (if (eq spec config)
        (cons nil spec)
      (setcdr tail nil)
      (cons config spec))))

(defun mole-make-prod-num-table (productions)
  "Create a hashtable for `mole-build-prod-nums' from PRODUCTIONS."
  (let ((table (make-hash-table :test 'eq)) (i 0))
    (dolist (p productions)
      (puthash p i table)
      (cl-incf i))
    table))

(defun mole-build-production (spec)
  "Return a (name args body) list for SPEC."
  (cl-destructuring-bind (name props args) spec
    (let* ((children (make-symbol "children"))
           (non-empty (cl-gensym "non-empty"))
           (mole-build-params (mapcar #'mole-munge-production-name (plist-get props :params)))
           (mole-build-lexical (plist-get props :lexical))
           (mole-build-fusing (plist-get props :fuse))
           (body `(mole-parse-match (,children ,(mole-build-sequence args))
                    ,(if (plist-get props :pass-thru)
                         `(let ((,non-empty (cl-remove-if-not #'mole-node-non-empty ,children)))
                            (cond
                             ((cdr ,non-empty) (mole-node ',name ,children ,mole-build-fusing))
                             (,non-empty (car ,non-empty))
                             (t (mole-node ',name ,children ,mole-build-fusing))))
                       `(mole-node ',name ,children ,mole-build-fusing))
                    'fail)))
      (unless mole-build-params
        (setq body `(mole-cached-result ,(gethash name mole-build-prod-nums) ,body)))
      (when mole-build-with-debug
        (setq body `(prog2
                        (push ',(mole-unmunge-production-name name) mole-debug-call-stack)
                        ,body
                      (pop mole-debug-call-stack))))
      (list name mole-build-params body))))

(defun mole-build-element (production)
  "Compile PRODUCTION into recursive calls."
  (cond
   ((symbolp production)
    (setq production (mole-munge-production-name production))
    (unless (or (gethash production mole-build-prod-nums)
                (memq production mole-build-params))
      (error "Production %s is not defined" production))
    `(funcall ,production))
   ((stringp production) `(mole-parse-anonymous-literal ,production))
   ((consp production)
    (pcase (car production)
      (': (mole-build-sequence-operator (cdr production)))
      ('or (mole-build-or (cdr production)))
      ('* (mole-build-zero-or-more (cdr production)))
      ('+ (mole-build-one-or-more (cdr production)))
      ((or '\? 'opt) (mole-build-zero-or-one (cdr production)))
      ((or '\?= '=) (mole-build-lookahead (cdr production)))
      ((or '\?! '!) (mole-build-negative-lookahead (cdr production)))
      ('char (mole-build-char (cdr production)))
      ('char-not (mole-build-char-not (cdr production)))
      ('lexical (mole-build-lexical (cdr production)))
      ('extern (mole-build-extern (cdr production)))
      ('with-context (mole-build-with-context (cdr production)))
      ('if-context (mole-build-if-context (cdr production)))
      ('\` (mole-build-symbol (cadr production)))
      ((pred numberp) (mole-build-repetition production))
      ((pred symbolp) (mole-build-parametric-call production))
      (_ (error "Unknown production %S" production))))
   (t (error "Unknown production %S" production))))

(defun mole-build-sequence (productions)
  "Compile PRODUCTIONS into sequenced calls to each.
The resulting form will be a list of `mole-node's and literals;
one for each item in productions.  If `mole-build-fusing' is
non-nil, all the descendant nodes are concatenated together into
a single string literal.

If `mole-build-lexical' is nil, whitespace is automatically
chomped (but not included in the resulting node) between each
product."
  (let ((res (make-symbol "res")))
    (if (null (cdr productions))
        ;; special-case single item sequences
        `(mole-parse-match (,res ,(mole-build-element (car productions)))
           (list ,res)
           'fail)
      (let ((block-name (make-symbol "block-name")))
        ;; ensure if any parse fails, go back to initial point
        `(mole-maybe-save-excursion
           (cl-block ,block-name
             (list
              (mole-parse-match (,res ,(mole-build-element (car productions)))
                ,res
                (cl-return-from ,block-name ,res))
              ,@(mapcar (lambda (prod)
                          `(mole-parse-match (,res
                                              (progn
                                                ,(unless mole-build-lexical `(mole-chomp-whitespace))
                                                ,(mole-build-element prod)))
                             ,res
                             (cl-return-from ,block-name ,res)))
                        (cdr productions)))))))))

(defun mole-build-sequence-operator (productions)
  "Like `mole-build-sequence', but returning a `mole-node-operator'.
PRODUCTIONS are the individual productions to match."
  (let ((res (make-symbol "res")))
    `(mole-parse-match (,res ,(mole-build-sequence productions))
       (mole-node ': ,res ,mole-build-fusing)
       'fail)))

(defun mole--unbouded-repeat-helper (productions star-items)
  "Helper for `mole-build-zero-or-more' and `mole-build-zero-or-one'.
PRODUCTIONS are the productions to match.
STAR-ITEMS is the symbol to use in the form for the parsed productions."
  (let ((item (make-symbol "item"))
        (production-form (mole-build-sequence productions)))
    `(let (,item)
       (while (mole-parse-success-p
               (setq ,item
                     ,(if mole-build-lexical
                          production-form
                        `(mole-maybe-save-excursion
                           (and ,star-items (mole-chomp-whitespace))
                           ,production-form))))
         (mole-debug (unless (cl-some #'mole-node-non-empty ,item)
                       (error "Infinite loop detected")))
         (setq ,star-items (nconc ,star-items ,item))))))

(defun mole-build-zero-or-more (productions)
  "Return a form that evaluates to zero or more PRODUCTIONS instances."
  (let ((star-items (make-symbol "star-items")))
    `(let (,star-items)
       ,(mole--unbouded-repeat-helper productions star-items)
       (mole-node '* ,star-items ,mole-build-fusing))))

(defun mole-build-one-or-more (productions)
  "Return a form that evaluates to one or more PRODUCTIONS instances."
  (let ((star-items (make-symbol "star-items")))
    `(let (,star-items)
       ,(mole--unbouded-repeat-helper productions star-items)
       (if ,star-items
           (mole-node '+ ,star-items ,mole-build-fusing)
         'fail))))

(defun mole-build-zero-or-one (productions)
  "Return a form that evaluates to zero or one PRODUCTIONS instances."
  (let ((res (make-symbol "res")))
    `(mole-node '\? (mole-parse-match (,res ,(mole-build-sequence productions))
                      ,res nil)
                ,mole-build-fusing)))

(defun mole-build-or (productions)
  "Return a form for evaluating a disjunction between PRODUCTIONS."
  (let ((child (make-symbol "child")))
    `(or ,@(mapcar (lambda (prod)
                     `(mole-parse-match (,child ,(mole-build-element prod))
                        ,child nil))
                   productions)
         'fail)))

(defun mole-build-lookahead (productions)
  "Return a form for evaluating PRODUCTIONS in `save-excursion'."
  `(mole-parse-match (res (save-excursion ,(mole-build-sequence productions)))
     (mole-node-literal (point) (point)) 'fail))

(defun mole-build-negative-lookahead (productions)
  "Return a form for evaluating (not PRODUCTIONS) in `save-excursion'."
  `(mole-parse-match (res (save-excursion ,(mole-build-sequence productions)))
     'fail (mole-node-literal (point) (point))))

(defun mole-build-repetition (productions)
  "Return a form for evaluating PRODUCTIONS multiple times."
  (let ((min (car productions))
        (max (cadr productions))
        (productions (cddr productions))
        (num (make-symbol "num"))
        (child (make-symbol "child"))
        (children (make-symbol "children"))
        production-form)
    (unless (numberp max)
      (push max productions)
      (setq max min))
    (setq production-form (mole-build-sequence productions))
    `(mole-maybe-save-excursion
       (let ((,num 0) ,child ,children)
         (while (and (< ,num ,max)
                     (mole-parse-success-p
                      (setq ,child
                            ,(if mole-build-lexical
                                 production-form
                               `(mole-maybe-save-excursion
                                  (or (zerop ,num) (mole-chomp-whitespace))
                                  ,production-form)))))
           (cl-callf nconc ,children ,child)
           (cl-incf ,num))
         (if (>= ,num ,min)
             (mole-node 'repetition ,children ,mole-build-fusing)
           'fail)))))

(defun mole-build-lexical (productions)
  "Return a form for evaluation PRODUCTIONS, but in a lexical environment."
  (let ((mole-build-lexical t)
        (res (make-symbol "res")))
    `(mole-parse-match (,res ,(mole-build-sequence productions))
       (mole-node 'lexical ,res ,mole-build-fusing)
       'fail)))

(defun mole-build-symbol (symbol)
  "Return a form for matching SYMBOL's name if surrounded by non-symbol characters."
  (let ((res (make-symbol "res")))
    `(if (not (looking-at-p (rx symbol-start)))
         (progn (mole-update-highwater-mark (point)) 'fail)
       (mole-parse-match (,res (mole-parse-anonymous-literal ,(symbol-name symbol)))
         (mole-maybe-save-excursion
           (if (looking-at-p (rx symbol-end))
               ,res
             'fail))))))

(defun mole-build-char (sets)
  "Return a form for matching SETS of characters, like using char in `rx'."
  `(if (looking-at (rx (char ,@sets)))
       (progn
         (mole-update-highwater-mark (point))
         (forward-char)
         (mole-node-literal (1- (point)) (point)))
     (mole-update-highwater-mark (point))
     'fail))

(defun mole-build-char-not (sets)
  "Return a form for matching characters not in SETS, like using (not (any ...)) in `rx'."
  `(if (looking-at (rx (not (any ,@sets))))
       (progn
         (mole-update-highwater-mark (point))
         (forward-char)
         (mole-node-literal (1- (point)) (point)))
     (mole-update-highwater-mark (point))
     'fail))

(cl-defun mole-build-with-context (((key value) &rest productions))
  "Add (KEY . VALUE) to the parse context and execute PRODUCTIONS."
  (cl-assert (symbolp key))
  (let ((res (make-symbol "res")))
    `(let ((mole-runtime-context (mole-context-set mole-runtime-context ',key ,value)))
       (mole-parse-match (,res ,(mole-build-sequence productions))
         (mole-node 'with-context ,res ,mole-build-fusing)
         'fail))))

(cl-defun mole-build-if-context (((key value) &rest productions))
  "If context[KEY] is VALUE, execute PRODUCTIONS."
  (cl-assert (symbolp key))
  (let ((res (make-symbol "res")))
    `(if (eq ,value (mole-context-get mole-runtime-context ',key))
         (mole-parse-match (,res ,(mole-build-sequence productions))
           (mole-node 'if-context ,res ,mole-build-fusing)
           'fail)
       'fail)))

(cl-defun mole-build-extern ((fn &rest args))
  "Build a custom matcher calling FN with ARGS."
  `(funcall ,fn ,@(mapcar (lambda (arg)
                            (if (symbolp arg)
                                (mole-munge-production-name arg)
                              arg))
                          args)))

(cl-defun mole-build-parametric-call ((prod &rest productions))
  "Build a parametric application of PROD with PRODUCTIONS as arguments."
  `(funcall ,(mole-munge-production-name prod)
            ,@(mapcar (lambda (p)
                        (cond
                         ((functionp p) p)
                         ((and (symbolp p) (gethash p mole-build-prod-nums)) p)
                         ((and (symbolp p) (memq p mole-build-params)) p)
                         ((eq (car-safe p) 'quote) p)
                         (t `(lambda () ,(mole-build-element p)))))
                      productions)))

(defmacro mole-create-grammar (&rest productions)
  "Create a new grammar object with PRODUCTIONS.

PRODUCTIONS is a list of (NAME &rest ARGS), where NAME is a
symbol and ARGS is a list with any of the following elements:

:params -- The following element must be a lambda arglist.  The
production created will be a parametric production, meaning its
parse result will not be cached and productions calling it must
call it with arguments.  This allows building higher-order
productions.

:lexical -- If the following argument is non-nil, whitespace will
not be automatically chomped between productions.

:fuse -- If the following argument is non-nil, all child nodes
will be fused into a single node in the resulting parse tree.

a symbol -- Must be the name of a production node which is
matched.

a string -- Will be matched literally

`symbol -- Will match the symbol name literally but only if
immediately surrounded by non-symbol characters.

\(char &rest char-spec\) -- Will match a regexp using the `rx'
char production.  (This only allows character classes, ranges and
literals).

\(char-not &rest char-spec\) -- A negated char match.  Like using
^ in a regexp.

\(: &rest prods\) -- Sequence operator.  Match PRODS in order and
succeed if they all succeed; otherwise fail.

\(or &rest prods\) -- Ordered choice operator.  Each production in
PRODS is tried in order until one of them succeeds.

\(* &rest prods\) -- Match PRODS sequentially zero or more times.
This production never fails.

\(+ &rest prods\) -- Match PRODS sequentially one or more times.

\(opt &rest prod\)
\(\? &rest prods\) -- Match PRODS sequentially zero or one time.
This production never fails.  (Note that the question mark must
be escaped for the Lisp reader).

\(= &rest prods\)
\(\?= &rest prods\) -- If PRODS match sequentially, this
production succeeds without advancing point and without
generating any output.  (Note that the question mark must be
escaped for the Lisp reader).

\(! &rest prods\)
\(\?! &rest prods\) -- If PRODS do not match sequentially, this
production succeeds without advancing point and without
generating any output.  (Note that the question mark must be
escaped for the Lisp reader).

\(NUMBER &rest prods\)
\(NUMBER NUMBER &rest prods\) -- Both forms repeatedly match
PRODS.  The first value is the minimum number of repetitions that
must match to succeed.  If the second value is a number, the form
will greedily try to parse beyond the minimal match until PRODS
stop matching or it reaches the maximum number of repetitions.

\(lexical &rest prods\) -- Match PRODS sequentially, without
chomping whitespace between productions.  Applies recursively to
sub-productions within PRODS.

\(with-context (key value) &rest prods\) -- Set KEY to VALUE in
the parse context and match PRODS sequentially.

\(if-context (key value) &rest prods\) -- If KEY is set to VALUE
in the parse context (compared with `eq'), match PRODS
sequentially.

\(extern fn &rest args\) -- Calls FN with ARGS to retrieve a
parse result.  ARGS is evaluated at grammar build time and can
refer to other productions by their names.  E.g.,
\(extern 'my-parse-func whitespace number) will call 'my-parse-func
with two arguments that can be funcalled to parse 'whitespace or
'number.

Any other (symbol &rest args) argument is considered a call to a
parametric production (one defined with `:params')."
  (unless (assq 'whitespace productions)
    (push mole-default-whitespace-terminal productions))
  (cl-callf mole-munge-productions productions)
  (unless (plist-get (cadr (assq 'mole~whitespace productions)) :lexical)
    (error "`whitespace' production must be lexical to avoid an infinite loop"))
  (let ((dups (mole-get-duplicates (mapcar #'car productions))))
    (when dups (error "Duplicate production definitions: %S" dups)))
  (let ((mole-build-prod-nums (mole-make-prod-num-table (mapcar 'car productions))))
    `(let (,@(mapcar 'car productions))
       ,@(mapcar (lambda (spec)
                   (cl-destructuring-bind (name args body) (mole-build-production spec)
                     `(setq ,name (lambda ,args ,body))))
                 productions)
       (make-mole-grammar :productions (list ,@(mapcar (lambda (term)
                                                         `(cons ',(car term) ,(car term)))
                                                       productions))))))

(defun mole-get-duplicates (list)
  "Return a list of unique items in LIST that appear more than once."
  (let (elt res)
    (while list
      (setq elt (car list) list (cdr list))
      (when (and (memq elt list) (not (memq elt res)))
        (push elt res)))
    (nreverse res)))

(defun mole-count-args (arg-list)
  "Return a (min . max) pair of argument counts matching ARG-LIST.
Only handles &optional and &rest annotations."
  (cl-block mole-count-args
    (let (min (i 0))
      (dolist (arg arg-list)
        (pcase arg
          ('&optional (setq min i))
          ('&rest (cl-return-from mole-count-args (cons (or min i) nil)))
          (_ (cl-incf i))))
      (cons (or min i) i))))

(defun mole-bind-args (arg-list call-args)
  "Return an alist of (arg . value) bindings.
The bindings correspond to what would be bound if calling a
function with ARG-LIST with CALL-ARGS."
  (apply `(lambda ,arg-list
            (list ,@(mapcar (lambda (a) `(list ',a ,a))
                            (remq '&rest (remq '&optional arg-list)))))
         call-args))

(defmacro mole-lazy (&rest body)
  "Turn BODY into a lazily-evaluated form.
Use `mole-unlazy' for obtaining the value.  BODY should be pure,
as the result is cached after the first evaluation."
  (declare (debug (&rest form)) (indent defun))
  (let ((called (make-symbol "called"))
        (res (make-symbol "res")))
    `(let (,called ,res)
       (lambda ()
         (if ,called ,res
           (setq ,called t
                 ,res (progn ,@body)))))))

(defalias 'mole-unlazy #'funcall "Force evaluation of a lazy value.")

(defun mole-populate-empty-table (productions)
  "Build a hashtable mapping PRODUCTIONS to whether they can match empty.
Productions match empty if they don't advance point on a
succesful match.  Also check for left-recursive loops and invalid
parametric production calls."
  (setq productions (mole--munge-productions-1 productions))
  (let ((empty-cache (make-hash-table :test #'eq)) call-stack call-args-stack)
    (cl-labels
        ((prod-matches-empty
          (name call-args)
          (when (memq name call-stack)
            (error "Detected left-recursive call to %s:\n%S" name call-stack))
          (push name call-stack)
          (prog1 (prod-matches-empty-1 name call-args)
            (pop call-stack)))
         (prod-matches-empty-1
          (name call-args)
          (cl-block prod-matches-empty-1
            (if-let (temp (assq name (car call-args-stack)))
                (if call-args (error "Parametric arguments are not supported:\n%S" call-stack)
                  (mole-unlazy (cdr temp)))
              (cl-destructuring-bind (_ args body)
                  (or (assq name productions) (error "Unknown production %s:\n%S" name call-stack))
                ;; Check arg count here to provide friendlier error message
                (cl-destructuring-bind (min . max) (mole-count-args args)
                  (let ((arg-count (length call-args)))
                    (cl-assert (and (>= arg-count min) (or (not max) (<= arg-count max)))
                               nil "Wrong number of arguments: %s. Exepected %S, got %s:\n%S"
                               name (cons min max) arg-count call-stack)))
                (unless args
                  (let ((cached (gethash name empty-cache 'not-found)))
                    (unless (eq cached 'not-found)
                      (cl-return-from prod-matches-empty-1 cached))))
                (push (mole-bind-args
                       args
                       (mapcar (lambda (term)
                                 (if (or (eq (car-safe term) 'quote) (functionp term))
                                     (mole-lazy (error "Quoted param can only be used with 'extern"))
                                   ;; TODO: Correctly handle case when call-args refer to the
                                   ;; calling productions call args
                                   (mole-lazy (term-matches-empty term))))
                               call-args))
                      call-args-stack)
                (let ((res (cl-every #'term-matches-empty body)))
                  (pop call-args-stack)
                  (unless args (setf (gethash name empty-cache) res))
                  res)))))

         (term-matches-empty
          (production)
          (cond
           ((symbolp production) (prod-matches-empty production nil))
           ((stringp production) (zerop (length production)))
           ((consp production)
            (pcase (car production)
              ((or '* '\? 'opt '\?! '! '\?= '=) t)
              ((or 'char 'char-not '\`) nil)
              ((or ': '+ 'lexical) (cl-every #'term-matches-empty (cdr production)))
              ((or 'with-context 'if-context)
               ;; There may be some false-positives in the cycle detection here since we don't
               ;; account for context changing. E.g.,
               ;; ((a (or (if-context (c 1) b) "x"))
               ;;  (b (if-context (c 1) (with-context (c 2) a))))
               (cl-every #'term-matches-empty (cddr production)))
              ('or (cl-some #'term-matches-empty (cdr production)))
              ('extern
               (dolist (arg (cddr production))
                 (when (symbolp arg)
                   (term-matches-empty arg)))
              ;; Conservatively assume extern can match empty.
              ;; TODO: Allow annotations to determine whether production matches or not
               t)
              ((pred numberp)
               (or (zerop (car production))
                   (cl-every #'term-matches-empty (if (numberp (cadr production))
                                                      (cddr production)
                                                    (cdr production)))))
              ((pred symbolp) (prod-matches-empty (car production) (cdr production)))
              (_ (error "Unknown production %S" production))))
           (t (error "Unknown production %S" production)))))
      (dolist (prod productions)
        (unless (plist-get (cadr prod) :params)
          (prod-matches-empty (car prod) nil))))
    empty-cache))

(defun mole-munge-production-name (prod)
  "Munge symbol PROD so that it does not conflict with existing variables.
Mole grammars let-bind productions, so if production names
conflict with dynamically bound symbols (e.g., debugger), trouble
can ensue."
  (intern (concat "mole~" (symbol-name prod))))

(defun mole-unmunge-production-name (munged)
  "Inverse of `mole-munge-production-name' applied to MUNGED."
  (intern (substring (symbol-name munged) 5)))

(defun mole-munge-productions (productions)
  "Munge PRODUCTIONS into an easier format to work with."
  (mole--munge-productions-2
   (mole--munge-productions-1 productions)))

(defun mole--munge-productions-1 (productions)
  "Munge PRODUCTIONS from the user-friendly format into a list of specs."
  (let ((defaults mole-default-props) prods)
    (while productions
      (if (memq (car productions) mole-production-keys)
          (let ((kw (pop productions)))
            (push (pop productions) defaults)
            (push kw defaults))
        (let* ((prod (pop productions))
               (arg-spec (mole-split-spec-args (cdr prod))))
          (push (list (car prod) ; name
                      (nconc (car arg-spec) defaults) ; props
                      (cdr arg-spec)) ; spec
                prods))))
    (nreverse prods)))

(defun mole--munge-productions-2 (productions)
  "Munge the names of PRODUCTIONS."
  (mapcar
   (lambda (prod) (cons (mole-munge-production-name (car prod)) (cdr prod)))
   productions))

(defun mole-parse (grammar production)
  "Attempt to parse GRAMMAR's PRODUCTION starting at point."
  (setq mole-debug-call-stack nil)
  (setq mole-runtime-string-parse nil)
  (save-excursion
    (let ((mole-runtime-highwater-mark (point))
          (mole-runtime-cache
           (mole-cache (length (mole-grammar-productions grammar)) #'mole-context-compare)))
     (if-let ((parser (assq (mole-munge-production-name production)
                            (mole-grammar-productions grammar))))
        (funcall (cdr parser))
      (error "Production %S not defined in grammar" production)))))

(defun mole-parse-string (grammar production string)
  "Attempt to parse GRAMMAR's PRODUCTION in STRING."
  (with-temp-buffer
    (insert string)
    (goto-char (point-min))
    (prog1 (mole-parse grammar production)
      (setq mole-runtime-string-parse string))))

(provide 'mole)

;;; mole.el ends here

;; Local Variables:
;; checkdoc-verb-check-experimental-flag: nil
;; End:
