;;; mole-tests --- Tests for mole.js  -*- lexical-binding: t -*-

;; Copyright (C) 2018 Felipe Ochoa

;; Author: Felipe Ochoa <felipe@fov.space>
;; URL: https://github.com/felipeochoa/mole/
;; Created: 10 Nov 2017
;; Package-Requires: ((emacs "25.2") (f "0.19.0"))
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

;;; Many of these tests use eval so that the macroexpansion happens at
;;; runtime.  Makes errors more clear in ERT, and it ensures the tests
;;; don't have to be reevaluated after every change to a build
;;; function.

;;; Code:

(require 'ert)
(require 'ert-x)
(require 'f)

(load (f-expand "mole" (f-parent (f-dirname (f-this-file)))))
(load (f-expand "mole-cache" (f-parent (f-dirname (f-this-file)))))
(load (f-expand "mole-context" (f-parent (f-dirname (f-this-file)))))


;; Test the helpers
(ert-deftest mole-name-munging ()
  "Ensure the symbol munging functions work correctly"
  (let ((cases '(abcd debugger mole-runtime-force-lexical)) munged)
    (dolist (case cases)
      (setq munged (mole-munge-production-name case))
      (should-not (eq munged case))
      (should (eq case (mole-unmunge-production-name munged))))))

(ert-deftest mole-node-clean-name ()
  "Test the unmunging of node names."
  (let ((node (make-mole-node :name (mole-munge-production-name 'abcd))))
    (should (eq (mole-node-clean-name node) 'abcd))))



;; Test productions

(ert-deftest mole-build-production-name ()
  "Ensure that `mole-build-production' assigns the correct name
  to the production."
  (let* ((productions '((mole~p1 nil ("p1")) (mole~p2 (:lexical t) ("a" "b")) (mole~p3 nil (p1 p2 p3))))
         (mole-build-prod-nums (mole-make-prod-num-table (mapcar 'car productions))))
    (dolist (prod productions)
      (should (eq (car prod)
                  (car (mole-build-production prod)))))))

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
         (fullname (intern (format "mole-builders-%s" firstname)))
         (munged (mole-munge-production-name firstname)))
    (unless (assq 'whitespace productions)
      (push mole-default-whitespace-terminal productions))
    (cl-callf mole-munge-productions productions)
    `(ert-deftest ,fullname ()
       (ert-with-test-buffer (:name ',fullname)
         ;; TODO: Figure out how to work the two levels of quasiquoting
         (eval
          (let ((mole-build-prod-nums (mole-make-prod-num-table ',(mapcar 'car productions))))
            (list
             'letrec (list
                      ,@(mapcar (lambda (p) `(list ',(car p)
                                                   (cons 'lambda (cdr (mole-build-production ',p)))))
                                productions)
                      '(mole-runtime-cache nil))
             '(dolist (succ ',successes)
                (unless (consp succ)
                  (setq succ (cons succ (1+ (length succ)))))
                (erase-buffer) (insert (car succ)) (goto-char (point-min))
                (setq mole-runtime-cache (make-mole-cache :num-prods ,(length productions)))
                (should (mole-parse-success-p (funcall ,munged)))
                (should (eq (point) (cdr succ))))
             ,(when failures
                `'(dolist (f ',failures)
                    (erase-buffer) (insert f) (goto-char (point-min))
                    (setq mole-runtime-cache (make-mole-cache :num-prods ,(length productions)))
                    (should (null (mole-parse-success-p (funcall ,munged))))
                    (should (bobp))))))
          t)))))

(mole-define-production-test ((sequence "t" (+ "e") "st"))
  ("teeeest")
  ("" "teeees"))

(mole-define-production-test ((sequence-operator (: "t" (+ "e") "st")))
  ("teeeest")
  ("" "teeees"))

(mole-define-production-test ((char (or char1 char2 char3 char4))
                              (char1 (char "ab"))
                              (char2 (char ?x ?y))
                              (char3 (char (?0 . ?9)))
                              (char4 (char "m-q")))
  (("ab" . 2) ("ba" . 2) ("xy" . 2) ("yx" . 2) "m" "n" "o" "p" "q")
  ("" "t"))

(mole-define-production-test ((char-class (+ (char hex-digit))))
  ("1234567890abcdef" "1234567890ABCDEF")
  ("" "xyz"))

(mole-define-production-test ((char-not (+ (char-not "abcd"))))
  ("\u4321" "efgh" ("efgha" . 5))
  ("" "a" "b" "c" "d"))

(mole-define-production-test ((zero-or-more (* "t" "a")))
  ("" "tatatata" ("xx" . 1)))

(mole-define-production-test ((one-or-more (+ "t" "a")))
  ("tatatata" "ta")
  ("" "xx" "at" "t" "a"))

(mole-define-production-test ((zero-or-one (\? "t" "a")))
  ("" "ta" ("tatatata" . 3) ("xx" . 1)))

(mole-define-production-test ((zero-or-one-2 (opt "t" "a")))
  ("" "ta" ("tatatata" . 3) ("xx" . 1)))

(mole-define-production-test ((or (or "t" "a")))
  ("t" "a" ("ta" . 2) ("at" . 2) ("tx" . 2) ("ax" . 2))
  ("" "x"))

(mole-define-production-test ((lookahead (= "te" "st")))
  (("test" . 1) ("testtest" . 1))
  ("" "tes" "te"))

(mole-define-production-test ((lookahead-2 (\?= "te" "st")))
  (("test" . 1) ("testtest" . 1))
  ("" "tes" "te"))

(mole-define-production-test ((negative-lookahead (! "te" "st")))
  ("" ("tes" . 1))
  ("test"))

(mole-define-production-test ((negative-lookahead (\?! "te" "st")))
  ("" ("tes" . 1))
  ("test"))

(mole-define-production-test ((whitespace-non-lexical "a" "b"))
  ("ab" "  ab  " " a   b  "))

(mole-define-production-test ((whitespace-backtracking :lexical t "a" (or nonterminal "b"))
                              (nonterminal "x"))
  ("ab" "a   x  ") ("a b"))

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

(mole-define-production-test ((force-failing-lexical (or lexi "abcd"))
                              (lexi (lexical "x" "y" "z")))
  ("abcd"))

(mole-define-production-test ((parametric (with-params "a" "b") (with-params "c" "d"))
                              (with-params :params (x y) (+ x) (2 y)))
  ("abbccccdd") ("" "abb" "cdd"))

(mole-define-production-test ((parametric-prod (with-params a b))
                              (a "a")
                              (b "b")
                              (with-params :params (x y) (+ x) (2 y)))
  ("aaabb" "abb") (""))

(mole-define-production-test ((parametric-recursive (with-params "a"))
                              (with-params :params (x) (with-params-2 x))
                              (with-params-2 :params (y) (+ y)))
  ("a" "aaaa") (""))

(mole-define-production-test ((parametric-nested (with-params-2 (with-params "a")))
                              (with-params :params (x) (+ x))
                              (with-params-2 :params (y) y "x"))
  ("ax" "aaaax") ("" "x"))

(mole-define-production-test ((non-caching-parametric (or (p "a") (p "b")))
                              (p :params (x) x))
  ("a" "b") ("" "x"))

(mole-define-production-test ((quoted-parametric (with-params (quote "abcd") (quote 123) 'abc))
                              (with-params :params (x y z)
                                           (extern (lambda (a1 a2 a3)
                                                     (should (string= a1 "abcd"))
                                                     (should (eq a2 123))
                                                     (should (eq a3 'abc))
                                                     'fail)
                                                   x y z)))
  () (""))

(ert-deftest mole-test-debug-call-stack ()
  "Ensure that when debugging the call stack is set correctly."
  (let (grammar)
    (let ((mole-build-with-debug t))
      (setq grammar (eval `(mole-create-grammar
                            (p1 p2)
                            (p2 p3 p4)
                            (p3 p5)
                            (p4 (extern (lambda ()
                                          (should (equal mole-debug-call-stack '(p4 p2 p1)))
                                          'fail)))
                            (p5 (extern (lambda ()
                                          (should (equal mole-debug-call-stack '(p5 p3 p2 p1)))
                                          'fail))))
                          t)))
    (mole-parse-string grammar 'p1 "")))

(ert-deftest mole-fuse-production ()
  "Ensure fusing productions join their children."
  (let* ((g (eval `(mole-create-grammar
                    (a :lexical t :fuse t "b" (* "c") (+ "d") (opt "e") (3 "f")))
                  t))
         res)
    (setq res (mole-parse-string g 'a "bccdfff"))
    (should (mole-parse-success-p res))
    (should (equal (mole-node-to-sexp res) '(a "bccdfff")))))

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
                                   (whitespace (* " "))
                                   (term "a"))))))
    (should (string-match-p "lexical" (cadr err)))))

(ert-deftest mole-basic-grammar-test ()
  "Test a very simple expression grammar."
  (let ((g (eval `(mole-create-grammar
                   (whitespace :lexical t (* (char " \t\n\f")))
                   (expression product (* (or "+" "-") product))
                   (product number (* (or "*" "/") number))
                   (number :fuse t (+ (char "0-9"))))
                 t)))
    (should (equal (mole-node-to-sexp (mole-parse-string g 'expression "3 \t+ 22 * 6"))
                   '(expression
                     (product (number "3"))
                     "+"
                     (product (number "22") "*" (number "6")))))))

(ert-deftest mole-duplicate-prods-test ()
  "Ensure duplicate productions are flagged at creation time."
  (let ((err (should-error (eval `(mole-create-grammar (x "x") (x "y")) t))))
    (should (string-match-p "duplicate" (cadr err)))))

(ert-deftest mole-unknown-prod-test ()
  "Ensure calling unknown productions fails at creation time."
  (let ((err (should-error (eval `(mole-create-grammar (x "x") (y "y" z)) t))))
    (should (string-match-p "defined" (cadr err)))))

(ert-deftest mole-grammar-varying-defaults ()
  "Test a grammar with various defaults plists"
  (let ((g (eval `(mole-create-grammar
                   :lexical t
                   (whitespace (* (char " \t\n\f")))
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

(defvar mole-test-hwm nil
  "Used to capture `mole-runtime-highwater-mark' during testing.")

(ert-deftest mole-highwater-mark-literal ()
  "Test that literals set the highwater mark correctly."
  (eval
   (let ((mole-build-prod-nums (mole-make-prod-num-table '(a whitespace))))
     `(let* ((mole~whitespace (lambda () nil))
             (a (lambda ,@(cdr (mole-build-production '(a () ("abc" (char (?d . ?f)))))))))
        (with-temp-buffer
          (dolist (fixture '(("abcd" . 4)
                             ("abce" . 4)
                             ("abc" . 4)
                             ("abcx" . 4)
                             ("abx" . 3)
                             ("x" . 1)
                             ("abcfxyz" . 4)))
            (erase-buffer)
            (insert (car fixture))
            (goto-char (point-min))
            (let ((mole-runtime-highwater-mark 0)
                  (mole-runtime-cache (make-mole-cache :num-prods 2)))
              (funcall a)
              (should (= mole-runtime-highwater-mark (cdr fixture))))))))
   t))

(ert-deftest mole-choice-highwatermark-choice ()
  "Ensure the highwater marks are set correctly in choice parsing."
  (eval
   (let ((mole-build-prod-nums (mole-make-prod-num-table (mapcar #'mole-munge-production-name
                                                                 '(whitespace a b choice)))))
     `(let* ((mole~whitespace (lambda () nil))
             (mole~a (lambda ,@(cdr (mole-build-production '(mole~a () ("x" "x" (+ "b") "a" "a"))))))
             (mole~b (lambda ,@(cdr (mole-build-production '(mole~b () ("xxb"))))))
             (choice (lambda ,@(cdr (mole-build-production '(mole~choice () ((or a b))))))))
        (with-temp-buffer
          (insert "xxbba")
          (let ((mole-runtime-highwater-mark 0)
                (mole-runtime-cache (make-mole-cache :num-prods 4)))
            (goto-char (point-min))
            (funcall choice)
            (should (= 6 mole-runtime-highwater-mark)))
          (let ((mole-runtime-highwater-mark 0)
                (mole-runtime-cache (make-mole-cache :num-prods 4)))
            (goto-char (point-min))
            (funcall mole~b)
            (should (= 3 mole-runtime-highwater-mark))))))))

(ert-deftest mole-cached-parse ()
  "Ensure that parsing uses the cache."
  (eval
   `(let* ((extern-fn-call-count 0)
           (extern-fn (lambda () (cl-incf extern-fn-call-count)
                        (make-mole-node-literal :pos (point) :end (point))))
           (g (mole-create-grammar
               (a e "a")
               (b (or (: a "x") (: a "y")))
               (e (extern extern-fn)))))

      (should (equal '(b (a (e "") "a") "y")
                     (mole-node-to-sexp (mole-parse-string g 'b "ay"))))
      (should (= 1 extern-fn-call-count)))
   t))

(ert-deftest mole-cached-parse-fail ()
  "Ensure that parsing uses the cache even when the result is fail."
  (eval
   `(let* ((extern-fn-call-count 0)
           (extern-fn (lambda () (cl-incf extern-fn-call-count)
                        (make-mole-node-literal :pos (point) :end (point))))
           (g (mole-create-grammar
               (a e "a")
               (b (or (: a "x") (: a "y") "b"))
               (e (extern extern-fn)))))

      (should (equal '(b "b") (mole-node-to-sexp (mole-parse-string g 'b "b"))))
      (should (= 1 extern-fn-call-count)))
   t))

(ert-deftest mole-cached-parse-with-context ()
  (eval
   `(let* ((extern-fn-call-count 0)
           (extern-fn (lambda () (cl-incf extern-fn-call-count)
                        (make-mole-node-literal :pos (point) :end (point))))
           (g (mole-create-grammar
               (a e "a")
               (b (or (: (with-context (k 'v) a) "x") (: (with-context (k 'v) a) "y")))
               (e (extern extern-fn)))))

      (should (equal '(b (a (e "") "a") "y")
                     (mole-node-to-sexp (mole-parse-string g 'b "ay"))))
      (should (= 1 extern-fn-call-count)))
   t))

(ert-deftest mole-cached-parse-context-mismatch ()
  (eval
   `(let* ((extern-fn-call-count 0)
           (extern-fn (lambda () (cl-incf extern-fn-call-count)
                        (make-mole-node-literal :pos (point) :end (point))))
           (g (mole-create-grammar
               (a e "a")
               (b (or (: (with-context (k 'v1) a) "x") (: (with-context (k 'v2) a) "y")))
               (e (extern extern-fn)))))
      (should (equal '(b (a (e "") "a") "y")
                     (mole-node-to-sexp (mole-parse-string g 'b "ay"))))
      (should (= 2 extern-fn-call-count)))
   t))

(mole-define-production-test ((with-context (with-context (x 123) (or p1 "aa" p2 "bb")))
                              (p1 (if-context (x 123) "a"))
                              (p2 (if-context (x 456) "b")))
  ("a" ("aa" . 2) "bb") ("" "b"))

(mole-define-production-test ((with-context-empty (or p1 "aa"))
                              (p1 (if-context (x 123) "a")))
  ("aa") ("" "a"))


(provide 'mole-tests)
;;; mole-test.el ends here
