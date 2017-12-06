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
  '(whitespace (:lexical t) (* (or " " "\t" "\n" "\f")))
  "If a grammar doesn't specify whitespace, this value will be used.")

(eval-and-compile
  (defvar mole-production-keys '(:lexical)
    "List of keys that may be given in a production definition.
:LEXICAL if nil, has productions chomp whitespace and comments
before attempting a match and after a successful match. If t,
no such chomping will be performed.")

  (defvar mole-default-props '()
    "Plist of `mole-production-keys' to use as defaults values
when creating a new grammar.")

  (defvar mole-runtime-force-lexical nil
    "If t, even non-lexical productions will not chomp whitespace.")
  )

(cl-defstruct mole-grammar productions)

(cl-defstruct mole-node name children)

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

(defmacro mole-maybe-save-excursion (&rest body)
  "Execute BODY and restore point unless return value is non-nil."
  (declare (debug (&rest form)) (indent defun))
  (let ((point (make-symbol "point")) (res (make-symbol "res")))
    `(let ((,point (point))
           (,res (progn ,@body)))
       (unless (mole-parse-success-p ,res)
         (goto-char ,point))
       ,res)))

(defun mole-parse-anonymous-literal (regexp)
  "Return a new anonymous literal if looking at REGEXP at point."
  (if (looking-at regexp)
      (progn (goto-char (match-end 0))
             (match-string-no-properties 0))
    'fail))

(defun mole-parse-success-p (result)
  "Return t if RESULT indicates a successful parse."
  (not (eq result 'fail)))

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
            (lexical (plist-get props :lexical)))
        (list name ()
              (if lexical
                  `(let ((,children ,(mole-build-sequence args)))
                     (if (mole-parse-success-p ,children)
                         (make-mole-node :name ',name :children ,children)
                       'fail))
                `(mole-maybe-save-excursion
                   (or mole-runtime-force-lexical (funcall whitespace))
                   (let ((,children ,(mole-build-sequence args)))
                     (if (mole-parse-success-p ,children)
                         (progn (or mole-runtime-force-lexical (funcall whitespace))
                                (make-mole-node :name ',name :children ,children))
                       'fail))))))))

  (defun mole-build-element (production)
    "Compile PRODUCTION into recursive calls."
    (cond
     ((symbolp production) `(funcall ,production))
     ((stringp production) `(mole-parse-anonymous-literal ,(regexp-quote production)))
     ((consp production)
      (pcase (car production)
        (': (mole-build-sequence-operator (cdr production)))
        ('or (mole-build-or (cdr production)))
        ('* (mole-build-zero-or-more (cdr production)))
        ('+ (mole-build-one-or-more (cdr production)))
        ('? (mole-build-zero-or-one (cdr production)))
        ('?= (mole-build-lookahead (cdr production)))
        ('?! (mole-build-negative-lookahead (cdr production)))
        ('lexical (mole-build-lexical (cdr production)))
        ('extern (mole-build-extern (cdr production)))
        ((pred numberp) (mole-build-repetition production))
        (_ (error "Unknown production %S" production))))
     (t (error "Unknown production %S" production))))

  (defun mole-build-sequence (productions)
    "Compile PRODUCTIONS into sequenced calls to each.
The resulting form will be a list of `mole-node's and literals;
one for each item in productions."
    (let ((res (make-symbol "res")))
      (if (null (cdr productions))
          ;; special-case single item sequences
          `(let ((,res ,(mole-build-element (car productions))))
             (if (mole-parse-success-p ,res)
                 (list ,res)
               ,res))
        (let ((block-name (make-symbol "block-name")))
          ;; ensure if any parse fails, go back to initial point
          `(mole-maybe-save-excursion
             (let (,res)
              (cl-block ,block-name
               (list ,@(mapcar (lambda (prod)
                                 `(if (mole-parse-success-p
                                       (setq ,res ,(mole-build-element prod)))
                                      ,res
                                    (cl-return-from ,block-name ,res)))
                               productions)))))))))

  (defun mole-build-sequence-operator (productions)
    "Like `mole-build-sequence', but returning a `mole-node-operator'."
    (let ((res (make-symbol "res")))
      `(let ((,res ,(mole-build-sequence productions)))
         (if (mole-parse-success-p ,res)
             (make-mole-node-operator :name ': :children ,res)
           ,res))))

  (defun mole-build-zero-or-more (productions)
    "Return a form that evaluates to zero or more PRODUCTIONS instances."
    (let ((item (make-symbol "item"))
          (star-items (make-symbol "star-items"))
          (production-form (mole-build-sequence productions)))
      `(let (,item ,star-items)
         (while (mole-parse-success-p (setq ,item ,production-form))
           (setq ,star-items (nconc ,star-items ,item)))
         (make-mole-node-operator :name '* :children ,star-items))))

  (defun mole-build-one-or-more (productions)
    "Return a form that evaluates to one or more PRODUCTIONS instances."
    (let ((item (make-symbol "item"))
          (star-items (make-symbol "star-items"))
          (production-form (mole-build-sequence productions)))
      `(let (,item ,star-items)
         (while (mole-parse-success-p (setq ,item ,production-form))
           (setq ,star-items (nconc ,star-items ,item)))
         (if ,star-items
             (make-mole-node-operator :name '+ :children ,star-items)
           'fail))))

  (defun mole-build-zero-or-one (productions)
    "Return a form that evaluates to zero or one PRODUCTIONS instances."
    (let ((res (make-symbol "res")))
      `(let ((,res ,(mole-build-sequence productions)))
         (make-mole-node-operator
          :name '?
          :children (if (mole-parse-success-p ,res) ,res nil)))))

  (defun mole-build-or (productions)
    "Return a form for evaluating a disjunction between productions."
    (let ((child (make-symbol "child")))
      `(let (,child)
         (or ,@(mapcar (lambda (prod)
                         `(when (mole-parse-success-p (setq ,child ,(mole-build-element prod)))
                            ,child))
                       productions)
             'fail))))

  (defun mole-build-lookahead (productions)
    "Return a form for evaluating PRODUCTIONS in `save-excursion'.
The form evaluates to \"\" if `production-form' evaluates to a
non-nil value, or nil otherwise."
    `(save-excursion
       (if (mole-parse-success-p ,(mole-build-sequence productions))
           ""
         'fail)))

  (defun mole-build-negative-lookahead (productions)
    "Return a form for evaluating PRODUCTION-FORM in `save-excursion'.
The form evaluates to \"\" if `production-form' evaluates to nil,
or nil otherwise."
    `(save-excursion
       (if (mole-parse-success-p ,(mole-build-sequence productions))
           'fail
         "")))

  (defun mole-build-repetition (productions)
    "Return a form for evaluating PRODUCTIONS multiple times.
The first value is the minimum number of repetitions. If the
second value is a number, it specifies a maximum number of
repetitions; otherwise the first value is taken as the max as
well."
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
             (push ,child ,children)
             (cl-incf ,num))
           (if (>= ,num ,min)
               (make-mole-node-operator :name 'repetition :children (nreverse ,children))
             'fail)))))

  (defun mole-build-lexical (productions)
    "Return a form for evaluation PRODUCTIONS, but in a lexical environment."
    `(let ((mole-runtime-force-lexical t))
       ,(mole-build-sequence productions)))

  (cl-defun mole-build-extern ((fn &rest args))
    "Build a custom matcher calling FN with ARGS.
ARGS is evaluated in the scope of the grammar builder, so it has
access to the productions, which are funcallable by their name."
    `(apply ,fn (list ,@args)))
  )

(defmacro mole-create-grammar (&rest productions)
  "Create a new grammar object with PRODUCTIONS."
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
