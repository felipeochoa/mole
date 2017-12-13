;;; mole-context.el --- Cacheable alists  -*- lexical-binding: t -*-

;; Copyright (C) 2017 Felipe Ochoa

;; Author: Felipe Ochoa <felipe@fov.space>
;; URL: https://github.com/felipeochoa/mole/
;; Created: 13 Dec 2017
;; Package-Requires: ((emacs "25.2"))
;; Version: 0.1
;; Keywords: maint

;; This file is not part of GNU Emacs.

;;; Commentary:
;;; This file includes utility functions for creating, updating, and
;;; comparing small key-value stores where the store itself is easily
;;; cacheable.  These stores are used in the mole packrat parser to
;;; efficiently cache parse results that may be context dependent.

;;; Code:

(defvar mole-context-compare--default (cons nil (make-symbol "not-found"))
  "Value used to distinguish not found from nil.")

(defun mole-context-compare (c1 c2)
  "Return t if C1 and C2 are equivalent contexts."
  (or (eq c1 c2)
      (and (= (length c1) (length c2))
           (let ((res t))
             (while (and res c1)
               (setq res (eq (cdar c1)
                             (cdr (or (assq (caar c1) c2)
                                      mole-context-compare--default)))
                     c1 (cdr c1)))
             res))))

(defsubst mole-context-get (ctx key)
  "Return the CTX's value for KEY."
  (cdr (assq key ctx)))

(defun mole-context-set (ctx key val)
  "Return a new context like CTX, but with KEY set to VAL."
  (cons
   (cons key val)
   (cond
    ((null ctx) nil)
    ((eq (caar ctx) key) (cdr ctx))
    (t (let ((tail ctx) (i 0) (res ctx))
         (while (setq i (1+ i) tail (cdr tail))
           (when (eq key (caar tail))
             (setq res nil)
             (dotimes (_ i) (push (pop ctx) res))
             (setq tail nil
                   res (nconc (nreverse res) (cdr tail)))))
         res)))))

(provide 'mole-context)

;;; mole-context.el ends here
