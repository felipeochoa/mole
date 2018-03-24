;;; mole.el --- Packrat parser generator  -*- lexical-binding: t -*-

;; Copyright (C) 2017 Felipe Ochoa

;; Author: Felipe Ochoa <felipe@fov.space>
;; URL: https://github.com/felipeochoa/mole/
;; Created: 10 Nov 2017
;; Package-Requires: ((emacs "25.2"))
;; Version: 0.1
;; Keywords: maint

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

(defvar mole-production-keys '(:lexical :fuse :params)
  "List of keys that may be given in a production definition.
:LEXICAL if nil, has productions chomp whitespace and comments
before attempting a match and after a successful match.  If t, no
such chomping will be performed.

:FUSE if non-nil all children will be merged into a single
node.  Productions using :FUSE cannot call other productions.

:PARAMS if non-nil should be an arglist suitable for a lambda
form.  Applications of this production must specify productions
to bind to this arglist, which can be referenced inside the rule
body.")

(defvar mole-default-props '()
  "Plist of `mole-production-keys' to use as defaults values.")

(defvar mole-operator-names '(: or * + \? \?= \?! opt = ! char lexical extern repetition
                                with-context if-context)
  "List of symbols reserved for mole operators.")

(defvar mole-runtime-force-lexical nil
  "If t, even non-lexical productions will not chomp whitespace.")

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
  "If t, literal parsing builders won't chomp whitespace.")

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

(cl-defstruct mole-grammar productions)

(cl-defstruct mole-node name children pos end)

(cl-defstruct (mole-node-operator (:include mole-node))
  "Node class for *, +, etc.")

(cl-defstruct (mole-node-literal (:include mole-node))
  "Node class for anonymous literals.") ;; just ignore name and children

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
  "Chomp whitespace unless `mole-runtime-force-lexical' is t."
  `(or mole-runtime-force-lexical
       (mole-ignore-hw-mark (funcall mole~whitespace))))

(defmacro mole-parse-anonymous-literal (string lexical)
  "Return a literal-parsing form for STRING.
STRING is the string to parse, LEXICAL is t if whitespace should
never be chomped.  (This second arg is used so that function
`mole-build-lexical' can be eagerly evaluated at build time.)"
  (declare (indent defun) (debug (stringp &or "t" "nil")))
  (if (= 1 (length string))
      `(if (eq (char-after) ,(aref string 0))
           (prog1 (mole-node-literal (point) (1+ (point)))
             (mole-update-highwater-mark (point))
             (forward-char)
             ,(unless lexical `(mole-chomp-whitespace)))
         (mole-update-highwater-mark (point))
         'fail)
    (let ((i (make-symbol "i")) (pos (make-symbol "pos")))
      `(mole-maybe-save-excursion
         (let ((,i 0) (,pos (point)))
           (while (and (< ,i ,(length string)) (eq (char-after) (aref ,string ,i)))
             (forward-char)
             (cl-incf ,i))
           (if (= ,i ,(length string))
               (prog1 (mole-node-literal ,pos (point))
                 (mole-update-highwater-mark (1- (point)))
                 ,(unless lexical `(mole-chomp-whitespace)))
             (mole-update-highwater-mark (point))
             'fail))))))

(eval-and-compile
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
             (mole-build-params (mapcar #'mole-munge-production-name (plist-get props :params)))
             (mole-build-lexical (plist-get props :lexical))
             (mole-build-fusing (plist-get props :fuse))
             (body `(mole-parse-match (,children ,(mole-build-sequence args))
                      (mole-node ',name ,children ,mole-build-fusing)
                      'fail)))
        (unless mole-build-lexical
          (setq body `(mole-maybe-save-excursion
                        (mole-chomp-whitespace)
                        (prog1 ,body
                          ;; Need to chomp again in case final production is lexical
                          (mole-chomp-whitespace)))))
        (unless mole-build-params
          (setq body `(mole-cached-result ,(gethash name mole-build-prod-nums) ,body)))
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
     ((stringp production) `(mole-parse-anonymous-literal ,production ,mole-build-lexical))
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
        ((pred numberp) (mole-build-repetition production))
        ((pred symbolp) (mole-build-parametric-call production))
        (_ (error "Unknown production %S" production))))
     (t (error "Unknown production %S" production))))

  (defun mole-build-sequence (productions)
    "Compile PRODUCTIONS into sequenced calls to each.
The resulting form will be a list of `mole-node's and literals;
one for each item in productions.  If `mole-build-fusing' is
non-nil, all the descendant nodes are concatenated together into
a single string literal."
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
                ,@(mapcar (lambda (prod)
                            `(mole-parse-match (,res ,(mole-build-element prod))
                               ,res
                               (cl-return-from ,block-name ,res)))
                          productions))))))))

  (defun mole-build-sequence-operator (productions)
    "Like `mole-build-sequence', but returning a `mole-node-operator'."
    (let ((res (make-symbol "res")))
      `(mole-parse-match (,res ,(mole-build-sequence productions))
         (mole-node ': ,res ,mole-build-fusing)
         'fail)))

  (defun mole-build-zero-or-more (productions)
    "Return a form that evaluates to zero or more PRODUCTIONS instances."
    (let ((item (make-symbol "item"))
          (star-items (make-symbol "star-items"))
          (production-form (mole-build-sequence productions)))
      `(let (,item ,star-items)
         (while (mole-parse-success-p (setq ,item ,production-form))
           (setq ,star-items (nconc ,star-items ,item)))
         (mole-node '* ,star-items ,mole-build-fusing))))

  (defun mole-build-one-or-more (productions)
    "Return a form that evaluates to one or more PRODUCTIONS instances."
    (let ((item (make-symbol "item"))
          (star-items (make-symbol "star-items"))
          (production-form (mole-build-sequence productions)))
      `(let (,item ,star-items)
         (while (mole-parse-success-p (setq ,item ,production-form))
           (setq ,star-items (nconc ,star-items ,item)))
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
    "Return a form for evaluating a disjunction between productions."
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
    "Return a form for evaluating (not PRODUCTION-FORM) in `save-excursion'."
    `(mole-parse-match (res (save-excursion ,(mole-build-sequence productions)))
       'fail (mole-node-literal (point) (point))))

  (defun mole-build-repetition (productions)
    "Return a form for evaluating PRODUCTIONS multiple times."
    (let ((min (car productions))
          (max (cadr productions))
          (productions (cddr productions))
          (num (make-symbol "num"))
          (child (make-symbol "child"))
          (children (make-symbol "children")))
      (unless (numberp max)
        (push max productions)
        (setq max min))
      `(mole-maybe-save-excursion
         (let ((,num 0) ,child ,children)
           (while (and (< ,num ,max)
                       (mole-parse-success-p (setq ,child ,(mole-build-sequence productions))))
             (cl-callf nconc ,children ,child)
             (cl-incf ,num))
           (if (>= ,num ,min)
               (mole-node 'repetition ,children ,mole-build-fusing)
             'fail)))))

  (defun mole-build-lexical (productions)
    "Return a form for evaluation PRODUCTIONS, but in a lexical environment."
    (let ((res (make-symbol "res")))
      `(let ((mole-runtime-force-lexical t))
         (mole-parse-match (,res ,(mole-build-sequence productions))
           (mole-node 'lexical ,res ,mole-build-fusing)))))

  (defun mole-build-char (sets)
    "Return a form for matching SETS of characters, like using char in `rx'."
    `(if (looking-at (rx (char ,@sets)))
         (prog1 (mole-node-literal (point) (1+ (point)))
           (mole-update-highwater-mark (point))
           (forward-char)
           ,(unless mole-build-lexical `(mole-chomp-whitespace)))
       (mole-update-highwater-mark (point))
       'fail))

  (defun mole-build-char-not (sets)
    "Return a form for matching characters not in SETS, like using (not (any ...)) in `rx'."
    `(if (looking-at (rx (not (any ,@sets))))
         (prog1 (mole-node-literal (point) (1+ (point)))
           (mole-update-highwater-mark (point))
           (forward-char)
           ,(unless mole-build-lexical `(mole-chomp-whitespace)))
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
    `(apply ,fn (list ,@(mapcar (lambda (arg)
                                  (if (symbolp arg)
                                      (mole-munge-production-name arg)
                                    arg))
                                args))))

  (cl-defun mole-build-parametric-call ((prod &rest productions))
    "Build a parametric application of PROD with PRODUCTIONS as arguments."
    `(funcall ,(mole-munge-production-name prod)
              ,@(mapcar (lambda (p)
                          (cond
                           ((functionp p) `,p)
                           ((and (symbolp p) (gethash p mole-build-prod-nums)) `,p)
                           ((and (symbolp p) (memq p mole-build-params)) `,p)
                           ((eq (car-safe p) 'quote) p)
                           (t `(lambda () ,(mole-build-element p)))))
                        productions))))

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
chomping whitespace between productions.

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
    (error "`whitespace' production must be lexical"))
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
  "Munge PRODUCTIONS from the user-friendly format into a list of specs."
  (let ((defaults mole-default-props) prods)
    (while productions
      (if (memq (car productions) mole-production-keys)
          (let ((kw (pop productions)))
            (push (pop productions) defaults)
            (push kw defaults))
        (let* ((prod (pop productions))
               (arg-spec (mole-split-spec-args (cdr prod))))
          (push (list (mole-munge-production-name (car prod)) ; name
                      (nconc (car arg-spec) defaults) ; props
                      (cdr arg-spec)) ; spec
                prods))))
    (nreverse prods)))

(defun mole-parse (grammar production)
  "Attempt to parse GRAMMAR's PRODUCTION starting at point."
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
