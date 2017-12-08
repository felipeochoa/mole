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

(defvar mole-default-whitespace-terminal
  '(whitespace (:lexical t :fuse t) ((* (char " \t\n\f"))))
  "If a grammar doesn't specify whitespace, this value will be used.")

(defvar mole-production-keys '(:lexical :fuse)
  "List of keys that may be given in a production definition.
:LEXICAL if nil, has productions chomp whitespace and comments
before attempting a match and after a successful match.  If t, no
such chomping will be performed.

:FUSE If non-nil all children will be merged into a single
string.  Productions using :FUSE cannot call other productions.")

(defvar mole-default-props '()
  "Plist of `mole-production-keys' to use as defaults values.")

(defvar mole-operator-names '(: or * + \? \?= \?! opt = ! char lexical extern repetition)
  "List of symbols reserved for mole operators.")

(defvar mole-runtime-force-lexical nil
  "If t, even non-lexical productions will not chomp whitespace.")

(defvar mole-build-fusing nil
  "Build-time dynamic variable to generate fusing nodes.")

(cl-defstruct mole-grammar productions)

(cl-defstruct mole-node name children)

(defmacro mole-node (name children &optional fuse)
  "Construct a `mole-node' instance named NAME with CHILDREN.
If FUSE is t, returns a string literal instead.  If NAME
indicates that the node should be an operator, a
`mole-node-operator' is created instead."
  (declare (debug (symbol form)))
  (cl-assert (and (consp name) (eq 'quote (car name))
                  (symbolp (setq name (eval name)))))
  (setq fuse (eval fuse))
  (cond
   ((memq name mole-operator-names)
    (if fuse
        `(apply #'concat ,children)
      `(make-mole-node-operator :name ',name :children ,children)))
   (fuse `(make-mole-node :name ',name :children (list (apply #'concat ,children))))
   (t `(make-mole-node :name ',name :children ,children))))

(cl-defstruct (mole-node-operator (:include mole-node))
  "Node class for *, +, etc.")

(cl-defmethod mole-node-to-sexp ((node mole-node))
  "Convert NODE into a test-friendly sexp."
  (cons (mole-node-name node)
        (cl-mapcan (lambda (child)
                     (if (mole-node-operator-p child)
                         (mole-node-to-sexp child)
                       (list (mole-node-to-sexp child))))
                   (mole-node-children node))))

(cl-defmethod mole-node-to-sexp ((literal string))
  "Convert LITERAL into a test-friendly sexp."
  literal)

(cl-defmethod mole-node-to-sexp ((_ (eql fail)))
  "Return 'fail if the parse failed."
  'fail)

(cl-defmethod mole-node-to-sexp ((op mole-node-operator))
  "Convert OP into a test-friendly sexp."
  (mapcar 'mole-node-to-sexp (mole-node-children op)))

(defun mole-parse-success-p (result)
  "Return t if RESULT indicates a successful parse."
  (not (eq result 'fail)))

(cl-defmacro mole-parse-match ((form res-sym) on-success &optional on-fail)
  "Execute FORM and conditionally execute a followup action.
RES-SYM is bound to the result of FORM.  If FORM is a successful
parse, execute ON-SUCCESS.  Otherwise execute ON-FAIL. ON-FAIL
defaults to simply returning 'fail."
  (declare (debug ((form symbol) form form)) (indent 1))
  `(let ((,res-sym ,form))
     (if (mole-parse-success-p ,res-sym)
         ,on-success
       ,on-fail)))

(defmacro mole-maybe-save-excursion (&rest body)
  "Execute BODY and restore point unless return value is non-nil."
  (declare (debug (&rest form)) (indent defun))
  (let ((point (make-symbol "point")) (res (make-symbol "res")))
    `(let ((,point (point))
           (,res (progn ,@body)))
       (unless (mole-parse-success-p ,res)
         (goto-char ,point))
       ,res)))

(defun mole-parse-anonymous-literal (string)
  "Return a new anonymous literal if looking at STRING at point."
  `(mole-maybe-save-excursion
     (let ((i 0) (len ,(length string)))
       (while (and (< i len) (eq (char-after) (aref ,string i)))
         (forward-char)
         (cl-incf i))
       (if (= i len) ,string 'fail))))

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

  (defun mole-build-production (spec)
    "Return a (name args body) list for SPEC."
    (cl-destructuring-bind (name props args) spec
      (let ((children (make-symbol "children"))
            (lexical (plist-get props :lexical))
            (mole-build-fusing (plist-get props :fuse)))
        (list name ()
              (if lexical
                  `(mole-parse-match (,(mole-build-sequence args) ,children)
                     (mole-node ',name ,children ,mole-build-fusing)
                     'fail)
                `(mole-maybe-save-excursion
                   (or mole-runtime-force-lexical (funcall whitespace))
                   (mole-parse-match (,(mole-build-sequence args) ,children)
                     (progn (or mole-runtime-force-lexical (funcall whitespace))
                            (mole-node ',name ,children ,mole-build-fusing))
                     'fail)))))))

  (defun mole-build-element (production)
    "Compile PRODUCTION into recursive calls."
    (cond
     ((symbolp production) `(funcall ,production))
     ((stringp production) (mole-parse-anonymous-literal production))
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
        ('lexical (mole-build-lexical (cdr production)))
        ('extern (mole-build-extern (cdr production)))
        ((pred numberp) (mole-build-repetition production))
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
          `(mole-parse-match (,(mole-build-element (car productions)) ,res)
             (list ,res)
             'fail)
        (let ((block-name (make-symbol "block-name")))
          ;; ensure if any parse fails, go back to initial point
          `(mole-maybe-save-excursion
             (cl-block ,block-name
               (list
                ,@(mapcar (lambda (prod)
                            `(mole-parse-match (,(mole-build-element prod) ,res)
                               ,res
                               (cl-return-from ,block-name ,res)))
                          productions))))))))

  (defun mole-build-sequence-operator (productions)
    "Like `mole-build-sequence', but returning a `mole-node-operator'."
    (let ((res (make-symbol "res")))
      `(mole-parse-match (,(mole-build-sequence productions) ,res)
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
      `(mole-node '\? (mole-parse-match (,(mole-build-sequence productions) ,res)
                        ,res nil)
                  ,mole-build-fusing)))

  (defun mole-build-or (productions)
    "Return a form for evaluating a disjunction between productions."
    (let ((child (make-symbol "child")))
      `(or ,@(mapcar (lambda (prod)
                       `(mole-parse-match (,(mole-build-element prod) ,child)
                          ,child nil))
                     productions)
           'fail)))

  (defun mole-build-lookahead (productions)
    "Return a form for evaluating PRODUCTIONS in `save-excursion'."
    `(save-excursion
       (mole-parse-match (,(mole-build-sequence productions) _)
         "" 'fail)))

  (defun mole-build-negative-lookahead (productions)
    "Return a form for evaluating (not PRODUCTION-FORM) in `save-excursion'."
    `(save-excursion
       (mole-parse-match (,(mole-build-sequence productions) _)
         'fail "")))

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
    `(let ((mole-runtime-force-lexical t))
       ,(mole-build-sequence productions)))

  (defun mole-build-char (sets)
    "Return a form for matching SETS of characters, like using char in `rx'."
    `(if (looking-at (rx (char ,@sets)))
         (progn (goto-char (match-end 0))
                (match-string-no-properties 0))
       'fail))

  (cl-defun mole-build-extern ((fn &rest args))
    "Build a custom matcher calling FN with ARGS."
    `(apply ,fn (list ,@args)))
  )

(defmacro mole-create-grammar (&rest productions)
  "Create a new grammar object with PRODUCTIONS.

PRODUCTIONS is a list of (NAME &rest ARGS), where NAME is a
symbol and ARGS is a list with any of the following elements:

a symbol -- Must be the name of a production node which is
matched.

a string -- Will be matched literally

\(char &rest char-spec\) -- Will match a regexp using the `rx'
char production.  (This only allows character classes, ranges and
literals).

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

\(extern fn &rest args\) -- Calls FN with ARGS to retrieve a
parse result.  ARGS is evaluated at grammar build time and can
refer to other productions by their names.  E.g.,
\(extern 'my-parse-func whitespace number) will call 'my-parse-func
with two arguments that can be funcalled to parse 'whitespace or
'number."
  (cl-callf mole-munge-productions productions)
  (let ((whitespace (assq 'whitespace productions)))
    (if whitespace
        (unless (plist-get (cadr whitespace) :lexical)
          (error "`whitespace' production must be lexical"))
      (push mole-default-whitespace-terminal productions)))
  `(let (,@(mapcar 'car productions))
     ,@(mapcar (lambda (spec)
                 (cl-destructuring-bind (name args body) (mole-build-production spec)
                   `(setq ,name (lambda ,args ,body))))
               productions)
     (make-mole-grammar :productions (list ,@(mapcar (lambda (term)
                                                       `(cons ',(car term) ,(car term)))
                                                     productions)))))

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
          (push (list (car prod) ; name
                      (nconc (car arg-spec) defaults) ; props
                      (cdr arg-spec)) ; spec
                prods))))
    (nreverse prods)))

(defun mole-parse (grammar production)
  "Attempt to parse GRAMMAR's PRODUCTION starting at point."
  (save-excursion
    (if-let ((parser (assq production (mole-grammar-productions grammar))))
        (funcall (cdr parser))
      (error "Production %S not defined in grammar" production))))

(defun mole-parse-string (grammar production string)
  "Attempt to parse GRAMMAR's PRODUCTION in STRING."
  (with-temp-buffer
    (insert string)
    (goto-char (point-min))
    (mole-parse grammar production)))

(provide 'mole)

;;; mole.el ends here
