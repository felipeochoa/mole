;;; mole-tests --- Tests for mole.js  -*- lexical-binding: t -*-
;; Package-Requires: ((emacs "25.2") (f "0.19.0"))

;;; Commentary:

;;; Code:

(require 'ert)
(require 'ert-x)
(require 'f)

(load (f-expand "mole" (f-parent (f-dirname (f-this-file)))))

(ert-deftest mole-builders-terminal ()
  (ert-with-test-buffer (:name 'mole-builders-terminal)
    (insert "teeeest")
    (goto-char (point-min))
    (let ((res (mole-build-terminal '(test "te+st"))))
      (should (eq (car res) 'test))
      (should (funcall `(lambda ,@(cdr res))))
      (should (eq (point) (point-max)))
      (save-excursion (insert "!teeeest"))
      (should (null (funcall `(lambda ,@(cdr res))))))))

(cl-defmacro mole-define-nonterminal-test (productions successes &optional failures)
  "Test that a nonterminal production matches correctly.
TESTNAME is used to name the production and the test.
PRODS are the productions that NAME should match.
SUCCESSES is a list of strings that NAME should parse.
FAILURES is a list of strings that NAME should not parse."
  (declare (indent 1))
  (cl-assert (and (listp productions) (every 'symbolp (mapcar 'car productions))))
  (cl-assert (and (listp successes) (every (lambda (s)
                                             (or (stringp s)
                                                 (and (consp s)
                                                      (stringp (car s))
                                                      (numberp (cdr s)))))
                                           successes)))
  (cl-assert (and (listp failures) (every 'stringp failures)))
  (let* ((firstname (caar productions))
         (fullname (intern (format "mole-builders-%s" firstname))))
    `(ert-deftest ,fullname ()
       (ert-with-test-buffer (:name ',fullname)
         ;; This test uses eval so that the macroexpansion happens at
         ;; runtime. This helps ERT deal with errors, and it ensures
         ;; the tests don't have to be recompiled after every change
         ;; to a build function.
         ;; TODO: Figure out how to work the two levels of quasiquoting
         (eval
          (list
           'letrec (list '(whitespace (lambda
                                        ,@(cdr (mole-build-terminal
                                                (list 'whitespace
                                                      mole-default-whitespace-terminal)))))
                         ,@(mapcar (lambda (p) `(list ',(car p)
                                                 (cons 'lambda (cdr (mole-build-nonterminal ',p)))))
                                   productions))
           '(dolist (succ ',successes)
              (unless (consp succ)
                (setq succ (cons succ (1+ (length succ)))))
              (erase-buffer) (insert (car succ)) (goto-char (point-min))
              (should (funcall ,firstname))
              (should (eq (point) (cdr succ))))
           ,(when failures
              `'(dolist (f ',failures)
                  (erase-buffer) (insert f) (goto-char (point-min))
                  (should (null (funcall ,firstname)))
                  (should (bobp)))))
          t)))))

(ert-deftest mole-build-nonterminal-name ()
  "Ensure that `mole-build-nonterminal' assigns the correct name
  to the production."
  (dolist (prod '((p1 "p1") (p2 "a" "b") (p3 a b c)))
    (should (eq (car prod)
                (car (mole-build-nonterminal prod))))))

(mole-define-nonterminal-test ((sequence "t" "e+" "st"))
  ("teeeest")
  ("" "teeees"))

(mole-define-nonterminal-test ((zero-or-more (* "t" "a")))
  ("" "tatatata" ("xx" . 1)))

(mole-define-nonterminal-test ((one-or-more (+ "t" "a")))
  ("tatatata" "ta")
  ("" "xx" "at" "t" "a"))

(mole-define-nonterminal-test ((zero-or-one (? "t" "a")))
  ("" "ta" ("tatatata" . 3) ("xx" . 1)))

(mole-define-nonterminal-test ((or (or "t" "a")))
  ("t" "a" ("ta" . 2) ("at" . 2) ("tx" . 2) ("ax" . 2))
  ("" "x"))

(mole-define-nonterminal-test ((lookahead (?= "te" "st")))
  (("test" . 1) ("testtest" . 1))
  ("" "tes" "te"))

(mole-define-nonterminal-test ((negative-lookahead (?! "te" "st")))
  ("" ("tes" . 1))
  ("test"))

(ert-deftest mole-basic-grammar-test ()
  "Test a very simple expression grammar."
  (let ((g (mole-create-grammar
            ((whitespace "[ \t\n\f]*")
             (number "[0-9]+"))
            ((expression product (* (or "\\+" "-") product))
             (product number (* (or "\\*" "/") number))))))
    (should (equal (mole-node-to-sexp (mole-parse-string g 'expression "3 + 2 * 6"))
                   '(expression
                     (product (number "3"))
                     "+"
                     (product (number "2") "*" (number "6")))))))

(provide 'mole-tests)
;;; mole-test.el ends here
