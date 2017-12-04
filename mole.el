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

(require 'eieio)
(require 'cl-lib)
(require 'subr-x)

(defclass mole-grammar ()
  ((productions :initarg :productions :accessor mole-grammar-productions)))

(defclass mole-node ()
  ((name :initarg :name :accessor mole-node-name)
   (children :initarg :children :accessor mole-node-children)))

(defclass mole-node-literal (mole-node)
  ((string :initarg :string :accessor mole-node-literal-string)))

(defclass mole-node-operator (mole-node) ()
  "Node class for *, +, etc.")

(cl-defmethod mole-node-to-sexp ((node mole-node))
  "Convert NODE into a test-friendly sexp."
  (cons (mole-node-name node)
        (cl-mapcan (lambda (child)
                     (if (mole-node-operator-p child)
                         (mole-node-to-sexp child)
                       (list (mole-node-to-sexp child))))
                   (mole-node-children node))))

(cl-defmethod mole-node-to-sexp ((literal mole-node-literal))
  "Convert LITERAL into a test-friendly sexp."
  (if (slot-boundp literal 'name)
      (list (mole-node-name literal) (mole-node-literal-string literal))
    (mole-node-literal-string literal)))

(cl-defmethod mole-node-to-sexp ((op mole-node-operator))
  "Convert OP into a test-friendly sexp."
  (mapcar 'mole-node-to-sexp (mole-node-children op)))

(defmacro mole-maybe-save-excursion (&rest body)
  "Execute BODY and restore point unless return value is non-nil."
  (declare (debug (&rest form)) (indent defun))
  (let ((point (make-symbol "point")))
    `(let ((,point (point)))
       (or (progn ,@body)
           (progn (goto-char ,point) nil)))))

(defun mole-parse-anonymous-literal (regexp)
  "If REGEXP matches at point, return a new anonymous literal."
  (when (looking-at regexp)
    (goto-char (match-end 0))
    (mole-node-literal :string (match-string-no-properties 0))))

(eval-and-compile
  (defvar mole-production-keys '(:lexical)
    "List of keys that may be given in a production definition.
:LEXICAL if nil, has productions chomp whitespace and comments
before attempting a match and after a successful match. If t,
no such chomping will be performed.")

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
    (cl-destructuring-bind (props . args) (mole-split-spec-args (cdr spec))
      (let ((name (car spec))
            (children (make-symbol "children"))
            (lexical (plist-get props :lexical)))
        (list name ()
              (if lexical
                  `(when-let (,children  ,(mole-build-sequence args))
                     (mole-node :name ',name :children ,children))
                `(mole-maybe-save-excursion
                   (funcall whitespace)
                   (when-let (,children  ,(mole-build-sequence args))
                     (funcall whitespace)
                     (mole-node :name ',name :children ,children))))))))

  (defun mole-build-element (production)
    "Compile PRODUCTION into recursive calls."
    (cond
     ((symbolp production) `(funcall ,production))
     ((stringp production) `(mole-parse-anonymous-literal ,production))
     ((consp production)
      (cl-case (car production)
        ('or (mole-build-or (cdr production)))
        ('* (mole-build-zero-or-more (cdr production)))
        ('+ (mole-build-one-or-more (cdr production)))
        ('? (mole-build-zero-or-one (cdr production)))
        ('?= (mole-build-lookahead (cdr production)))
        ('?! (mole-build-negative-lookahead (cdr production)))
        (t (error "Unknown production %S" production))))
     (t (error "Unknown production %S" production))))

  (defun mole-build-sequence (productions)
    "Compile PRODUCTIONS into sequenced calls to each.
The resulting form always evaluates to a list. If the sequence of
productions failed, the list will be nil. Otherwise, it will have
one `mole-node' for each item in productions."
    (let ((res (make-symbol "res")))
      (if (null (cdr productions))
          ;; special-case single item sequences
          `(when-let ((,res ,(mole-build-element (car productions)))) (list ,res)))
      (let ((block-name (make-symbol "block-name")))
        ;; ensure if any parse fails, go back to initial point
        `(mole-maybe-save-excursion
           (cl-block ,block-name
             (list ,@(mapcar (lambda (prod)
                               `(or ,(mole-build-element prod)
                                    (cl-return-from ,block-name)))
                             productions)))))))

  (defun mole-build-zero-or-more (productions)
    "Return a form that evaluates to zero or more PRODUCTIONS instances."
    (let ((item (make-symbol "item"))
          (star-items (make-symbol "star-items"))
          (production-form (mole-build-sequence productions)))
      `(let (,item ,star-items)
         (while (setq ,item ,production-form)
           (setq ,star-items (nconc ,star-items ,item)))
         (mole-node-operator :name '* :children ,star-items))))

  (defun mole-build-one-or-more (productions)
    "Return a form that evaluates to one or more PRODUCTIONS instances."
    (let ((item (make-symbol "item"))
          (star-items (make-symbol "star-items"))
          (production-form (mole-build-sequence productions)))
      `(let (,item ,star-items)
         (while (setq ,item ,production-form)
           (setq ,star-items (nconc ,star-items ,item)))
         (when ,star-items
           (mole-node-operator :name '+ :children ,star-items)))))

  (defun mole-build-zero-or-one (productions)
    "Return a form that evaluates to zero or one PRODUCTIONS instances."
    (let ((production-form (mole-build-sequence productions)))
      `(mole-node-operator :name '? :children ,production-form)))

  (defun mole-build-or (productions)
    "Return a form for evaluating a disjunction between productions."
    `(or ,@(mapcar 'mole-build-element productions)))

  (defun mole-build-lookahead (productions)
    "Return a form for evaluating PRODUCTIONS in `save-excursion'.
The form evaluates to a blank `mole-node-literal' if
`production-form' evaluates to a non-nil value, or nil
otherwise."
    `(save-excursion
       (when ,(mole-build-sequence productions)
         (mole-node-literal :name '?= :string ""))))

  (defun mole-build-negative-lookahead (productions)
    "Return a form for evaluating PRODUCTION-FORM in `save-excursion'.
The form evaluates to a blank `mole-node-literal' if
`production-form' evaluates to nil, or nil otherwise."
    `(save-excursion
       (unless ,(mole-build-sequence productions)
         (mole-node-literal :name '?= :string "")))))

(defvar mole-default-whitespace-terminal '(whitespace :lexical t "[ \t\n\f]*")
  "If a grammar doesn't specify whitespace, this value will be used.")

(defmacro mole-create-grammar (&rest productions)
  "Create a new grammar object with PRODUCTIONS."
  (unless (cl-some (lambda (term) (eq 'whitespace (car term))) productions)
    (push mole-default-whitespace-terminal productions))
  `(let (,@(mapcar 'car productions))
     ,@(mapcar (lambda (spec)
                 (cl-destructuring-bind (name args body) (mole-build-production spec)
                   `(setq ,name (lambda ,args ,body))))
               productions)
     (mole-grammar :productions (list ,@(mapcar (lambda (term)
                                                  `(cons ',(car term) ,(car term)))
                                                productions)))))

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
