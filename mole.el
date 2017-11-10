;;; mole.el --- Packrat parser generator  -*- lexical-binding: t -*-

;; Copyright (C) 2017 Felipe Ochoa

;; Author: Felipe Ochoa <felipe@fov.space>
;; URL: https://github.com/felipeochoa/mole/
;; Created: 10 Nov 2017
;; Package-Requires: ((emacs "24.4"))
;; Version: 0.1
;; Keywords: maint

;; This file is not part of GNU Emacs.

;;; Commentary:

;;; Code:

(require 'eieio)
(require 'cl-lib)
(require 'subr-x)

(defclass mole-grammar ()
  ((terminals :initarg :terminals :accessor mole-grammar-terminals)
   (nonterminals :initarg :nonterminals :accessor mole-grammar-nonterminals)))

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


(defun mole-parse-anonymous-literal (regexp)
  "If REGEXP matches at point, return a new anonymous literal."
  (when (looking-at regexp)
    (goto-char (match-end 0))
    (mole-node-literal :string (match-string-no-properties 0))))

(eval-and-compile
  (defun mole-build-terminal (spec)
    "Return a (name args body) list for SPEC."
    (cl-destructuring-bind (name re-or-kw &optional arg) spec
      (list name ()
            (cond
             ((stringp re-or-kw) `(when (looking-at ,re-or-kw)
                                    (goto-char (match-end 0))
                                    (mole-node-literal :name ',name :string
                                                       (match-string-no-properties 0))))
             ((eq 'extern re-or-kw) `(funcall ,arg))
             (t (error "Unknown terminal type %S" re-or-kw))))))

  (defun mole-build-nonterminal (spec)
    "Return a (name args body) list for SPEC."
    (let ((name (car spec))
          (args (cdr spec))
          (children (cl-gensym)))
      (list name ()
            `(when-let ((,children ,(mole-build-sequence args)))
               (mole-node :name ',name :children ,children)))))

  (defun mole-build-production (production)
    "Compile PRODUCTION into recursive calls."
    (cond
     ((symbolp production) `(prog2
                                (funcall whitespace)
                                (funcall ,production)
                              (funcall whitespace)))
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
    "Compile PRODUCTIONS into sequenced calls to each."
    (if (= 1 (length productions))
        (mole-build-production (car productions))
      (let* ((block-name (cl-gensym)))
        `(cl-block ,block-name
           (list ,@(mapcar (lambda (prod)
                             `(or ,(mole-build-production prod)
                                  (cl-return-from ,block-name)))
                           productions))))))

  (defun mole-build-zero-or-more (productions)
    "Return a form that evaluates to zero or more PRODUCTIONS instances."
    (let ((item (cl-gensym))
          (star-items (cl-gensym))
          (production-form (mole-build-sequence productions)))
      `(let (,item ,star-items)
         (while (setq ,item ,production-form)
           (setq ,star-items (nconc ,star-items ,item)))
         (mole-node-operator :name '* :children ,star-items))))

  (defun mole-build-one-or-more (productions)
    "Return a form that evaluates to one or more PRODUCTIONS instances."
    (let ((item (cl-gensym))
          (star-items (cl-gensym))
          (production-form (mole-build-sequence productions)))
      `(let (,item ,star-items)
         (while (setq ,item ,production-form)
           (setq ,star-items (nconc ,star-items ,item)))
         (when ,star-items
           (mole-node-operator :name '+ :children ,star-items)))))

  (defun mole-build-zero-or-one (productions)
    "Return a form that evaluates to zero or one PRODUCTIONS instances."
    (let ((item (cl-gensym))
          (production-form (mole-build-sequence productions)))
      `(mole-node-operator :name '? :children ,production-form)))

  (defun mole-build-or (productions)
    "Return a form for evaluating a disjunction between productions."
    `(or ,@(mapcar 'mole-build-production productions)))

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

(defvar mole-default-whitespace-terminal "[ \t\n\f]*"
  "If a grammar doesn't specify whitespace, this value will be used.")

(defmacro mole-create-grammar (terminals nonterminals)
  "Create a new grammar object with TERMINALS and NONTERMINALS."
  (unless (cl-some (lambda (term) (eq 'whitespace (car term))) terminals)
    (push mole-default-whitespace-terminal terminals))
  `(lexical-let (,@(mapcar 'car nonterminals) ,@(mapcar 'car terminals))
     ,@(mapcar (lambda (spec)
                 (cl-destructuring-bind (name args body) (mole-build-terminal spec)
                   `(setq ,name (lambda ,args ,body))))
               terminals)
     ,@(mapcar (lambda (spec)
                 (cl-destructuring-bind (name args body) (mole-build-nonterminal spec)
                   `(setq ,name (lambda ,args ,body))))
               nonterminals)
     (mole-grammar :terminals (list ,@(mapcar (lambda (term)
                                                `(cons ',(car term) ,(car term)))
                                              terminals))
                   :nonterminals (list ,@(mapcar (lambda (term)
                                                   `(cons ',(car term) ,(car term)))
                                                 nonterminals)))))

(defun mole-parse (grammar production)
  "Attempt to parse GRAMMAR's PRODUCTION starting at point."
  (save-excursion
    (or
     (when-let ((parser (assq production (mole-grammar-terminals grammar))))
       (funcall (cdr parser)))
     (when-let ((parser (assq production (mole-grammar-nonterminals grammar))))
       (funcall (cdr parser)))
     (error "Production %S not defined in grammar" production))))

(defun mole-parse-string (grammar production string)
  "Attempt to parse GRAMMAR's PRODUCTION in STRING."
  (with-temp-buffer
    (insert string)
    (goto-char (point-min))
    (mole-parse grammar production)))

(provide 'mole)

;;; mole.el ends here
