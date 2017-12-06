;;; mole-tests --- Tests for mole.js  -*- lexical-binding: t -*-
;; Package-Requires: ((emacs "25.2") (f "0.19.0"))

;;; Commentary:

;;; Many of these tests use eval so that the macroexpansion happens at
;;; runtime.  Makes errors more clear in ERT, and it ensures the tests
;;; don't have to be reevaluated after every change to a build
;;; function.

;;; Code:

(require 'ert)
(require 'ert-x)
(require 'f)

(load (f-expand "mole" (f-parent (f-dirname (f-this-file)))))

(ert-deftest mole-build-production-name ()
  "Ensure that `mole-build-production' assigns the correct name
  to the production."
  (dolist (prod '((p1 nil ("p1")) (p2 (:lexical t) ("a" "b")) (p3 nil (a b c))))
    (should (eq (car prod)
                (car (mole-build-production prod))))))

(cl-defmacro mole-define-production-test (productions successes &optional failures)
  "Test that a production production matches correctly.
TESTNAME is used to name the production and the test.
PRODS are the productions that NAME should match.
SUCCESSES is a list of strings that NAME should parse.
FAILURES is a list of strings that NAME should not parse."
  (declare (indent 1))
  (cl-assert (and (listp productions) (cl-every 'symbolp (mapcar 'car productions))))
  (cl-assert (and (listp successes) (cl-every (lambda (s)
                                                (or (stringp s)
                                                    (and (consp s)
                                                         (stringp (car s))
                                                         (numberp (cdr s)))))
                                              successes)))
  (cl-assert (and (listp failures) (cl-every 'stringp failures)))
  (let* ((firstname (caar productions))
         (fullname (intern (format "mole-builders-%s" firstname))))
    (unless (assq 'whitespace productions)
      (push (apply 'append
                   (list (car mole-default-whitespace-terminal))
                   (cdr mole-default-whitespace-terminal))
            productions))
    (cl-callf mole-munge-productions productions)
    `(ert-deftest ,fullname ()
       (ert-with-test-buffer (:name ',fullname)
         ;; TODO: Figure out how to work the two levels of quasiquoting
         (eval
          (list
           'letrec (list
                    ,@(mapcar (lambda (p) `(list ',(car p)
                                                 (cons 'lambda (cdr (mole-build-production ',p)))))
                              productions))
           '(dolist (succ ',successes)
              (unless (consp succ)
                (setq succ (cons succ (1+ (length succ)))))
              (erase-buffer) (insert (car succ)) (goto-char (point-min))
              (should (mole-parse-success-p (funcall ,firstname)))
              (should (eq (point) (cdr succ))))
           ,(when failures
              `'(dolist (f ',failures)
                  (erase-buffer) (insert f) (goto-char (point-min))
                  (should (null (mole-parse-success-p (funcall ,firstname))))
                  (should (bobp)))))
          t)))))

(mole-define-production-test ((sequence "t" "e+" "st"))
  ("teeeest")
  ("" "teeees"))

(mole-define-production-test ((sequence-operator (: "t" "e+" "st")))
  ("teeeest")
  ("" "teeees"))

(mole-define-production-test ((zero-or-more (* "t" "a")))
  ("" "tatatata" ("xx" . 1)))

(mole-define-production-test ((one-or-more (+ "t" "a")))
  ("tatatata" "ta")
  ("" "xx" "at" "t" "a"))

(mole-define-production-test ((zero-or-one (? "t" "a")))
  ("" "ta" ("tatatata" . 3) ("xx" . 1)))

(mole-define-production-test ((or (or "t" "a")))
  ("t" "a" ("ta" . 2) ("at" . 2) ("tx" . 2) ("ax" . 2))
  ("" "x"))

(mole-define-production-test ((lookahead (?= "te" "st")))
  (("test" . 1) ("testtest" . 1))
  ("" "tes" "te"))

(mole-define-production-test ((negative-lookahead (?! "te" "st")))
  ("" ("tes" . 1))
  ("test"))

(mole-define-production-test ((whitespace-backtracking "a" (or nonterminal "b"))
                              (nonterminal "x"))
  ("ab" "  ab  " " a   x  ") ("a b"))

(mole-define-production-test ((lexical lexical-callee lexical-callee)
                              (lexical-callee :lexical t "a"))
  ("aa" " aa" "  aa  ") ("a a"))

(defun mole-extern-test-fn (num production)
  "Test function for the extern element.
NUM PRODUCTION: appease flycheck."
  (should (= num 1))
  (mole-maybe-save-excursion
    (or
     (let (r1 r2 r3)
       (setq r1 (funcall production))
       (when (and r1 (looking-at-p "xyz"))
         (setq r2 "xyz") (forward-char 3)
         (setq r3 (funcall production))
         (when r3 (make-mole-node :name 'custom-name :children (list r1 r2 r3)))))
     'fail)))

(mole-define-production-test ((extern (extern 'mole-extern-test-fn 1 non-lexical))
                              (non-lexical "a"))
  ("axyza") ("a a" ""))

(mole-define-production-test ((repeat (2 lexical) (1 3 non-lexical))
                              (lexical :lexical t "a")
                              (non-lexical "t"))
  ("aat" "aatt" "aattt" "aa  t  t  t  " "aa  tt t" ("aatttt" . 6))
  ("" "at" "aaat" "a att"))

(mole-define-production-test ((force-lexical (lexical (2 non-lexical)) "|" (2 non-lexical))
                              (non-lexical "a"))
  ("aa| a a") ("a a| a a"))

(ert-deftest mole-split-spec-args ()
  "Ensure `mole-split-spec-args' works correctly."
  (dolist (fixture '((("a" "b" "c") () ("a" "b" "c"))
                     ((:lexical t "a" "b") (:lexical t) ("a" "b"))))
    (cl-destructuring-bind (input config production) fixture
      (should (equal (cons config production)
                     (mole-split-spec-args input))))))

(ert-deftest mole-whitespace-nonlexical-error ()
  "Ensure `mole-create-grammar' errors if whitespace is non-lexical."
  (let ((err (should-error (eval `(mole-create-grammar
                                   (whitespace " *")
                                   (term "a"))))))
    (should (string-match-p "lexical" (cadr err)))))

(ert-deftest mole-basic-grammar-test ()
  "Test a very simple expression grammar."
  (let ((g (eval '(mole-create-grammar
                   (whitespace :lexical t "[ \t\n\f]*")
                   (expression product (* (or "\\+" "-") product))
                   (product number (* (or "\\*" "/") number))
                   (number "[0-9]+"))
                 t)))
    (should (equal (mole-node-to-sexp (mole-parse-string g 'expression "3 + 2 * 6"))
                   '(expression
                     (product (number "3"))
                     "+"
                     (product (number "2") "*" (number "6")))))))

(ert-deftest mole-grammar-varying-defaults ()
  "Test a grammar with various defaults plists"
  (let ((g (eval `(mole-create-grammar
                   :lexical t
                   (whitespace "[ \t\n\f]*")
                   (a "a")
                   :lexical nil
                   (b "b")
                   (c "c")
                   :lexical t
                   (d "d"))
                 t)))

    (should (equal (mole-node-to-sexp (mole-parse-string g 'a "a")) '(a "a")))
    (should (equal (mole-node-to-sexp (mole-parse-string g 'a " a")) 'fail))
    (should (equal (mole-node-to-sexp (mole-parse-string g 'b " b  ")) '(b "b")))
    (should (equal (mole-node-to-sexp (mole-parse-string g 'c " c  ")) '(c "c")))
    (should (equal (mole-node-to-sexp (mole-parse-string g 'd "d")) '(d "d")))
    (should (equal (mole-node-to-sexp (mole-parse-string g 'd " d")) 'fail))))

(provide 'mole-tests)
;;; mole-test.el ends here
