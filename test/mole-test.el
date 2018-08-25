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
  (let ((cases '(abcd debugger mole-runtime-context)) munged)
    (dolist (case cases)
      (setq munged (mole-munge-production-name case))
      (should-not (eq munged case))
      (should (eq case (mole-unmunge-production-name munged))))))

(ert-deftest mole-node-clean-name ()
  "Test the unmunging of node names."
  (let ((node (make-mole-node :name (mole-munge-production-name 'abcd))))
    (should (eq (mole-node-clean-name node) 'abcd))))

(ert-deftest mole-node-visit ()
  "Test the visiting of nodes."
  (eval
   `(let* ((grammar (mole-create-grammar
                     (x "x")
                     (y (\? "y"))
                     (z (+ "z"))
                     (xyz (or (: x y z) (: x y) x))))
           (ast (mole-parse-string grammar 'xyz "xyzzz"))
           visits)
      (mole-visit ast (lambda (node end-p)
                        (push (cons (mole-node-clean-name node) end-p) visits)))
      (cl-callf nreverse visits)
      (should (equal (mole-node-to-sexp ast)
                     '(xyz (x "x") (y "y") (z "z" "z" "z"))))
      (should (equal visits '((xyz . nil)
                              (x . nil)
                              (x . t)
                              (y . nil)
                              (y . t)
                              (z . nil)
                              (z . t)
                              (xyz . t)))))
   t))

(ert-deftest mole-node-non-empty ()
  (should (mole-node-non-empty (make-mole-node :name 'xyz :pos 13 :end 14)))
  (should-not (mole-node-non-empty (make-mole-node :name 'xyz :pos 1 :end 1))))


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
                                                   `(lambda ,@(cdr (mole-build-production ',p)))))
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
  ("" "tatatata" "ta ta tata" ("xx" . 1)))

(mole-define-production-test ((zero-or-more-lexical :lexical t (* "t" "a")))
  ("" "tatatata" ("ta ta" . 3) ("tata ta" . 5) ("xx" . 1)))

(mole-define-production-test ((one-or-more (+ "t" "a")))
  ("tatatata" "ta ta tata" "ta")
  ("" "xx" "at" "t" "a"))

(mole-define-production-test ((one-or-more-lexical :lexical t (+ "t" "a")))
  ("tatatata" ("ta ta" . 3) ("tata ta" . 5))
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
  ("ab" "a  b") (" a   b  "))

(mole-define-production-test ((whitespace-backtracking :lexical t "a" (or nonterminal "b"))
                              (nonterminal "x"))
  ("ab" "ax") ("a b" "a   x" "a   x  "))

(mole-define-production-test ((lexical lexical-callee lexical-callee)
                              (lexical-callee :lexical t "a" "b"))
  ("abab" "ab ab") (" abab" "a b ab"))

(mole-define-production-test ((lexical-not-runtime :lexical t (lexical non-lexical non-lexical))
                              (non-lexical "a" "b"))
  ("abab" "a ba b") ("ab ab"))

(mole-define-production-test ((symbol (\? "x") the-symbol (\? "x"))
                              (the-symbol `abcd))
  ("abcd" "x abcd x") ("xabcd" "abcdx" "xabcdx"))

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

(mole-define-production-test ((repeat (lexical (2 lexical)) (1 3 non-lexical))
                              (lexical "a")
                              (non-lexical "t"))
  ("aat" "aatt" "aattt" ("aa  t  t  t  " . 12) "aa  tt t" ("aatttt" . 6))
  ("" "at" "aaat" "a att"))

(mole-define-production-test ((force-lexical (lexical (2 non-lexical)) "|" (2 non-lexical))
                              (non-lexical "a"))
  ("aa|aa" "aa|a a" "aa | a a") ("a a| a a"))

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

(mole-define-production-test ((quoted-parametric (with-params '"abcd" '123 'abc))
                              (with-params :params (x y z)
                                           (extern (lambda (a1 a2 a3)
                                                     (should (string= a1 "abcd"))
                                                     (should (eq a2 123))
                                                     (should (eq a3 'abc))
                                                     'fail)
                                                   x y z)))
  () (""))

(ert-deftest mole-pass-thru ()
  "Test :pass-thru."
  (eval
   `(let ((grammar (mole-create-grammar
                    (xyz :pass-thru t x y (\? z))
                    (x (\? "x"))
                    (y (\? "y"))
                    (z "z"))))
      (should (equal (mole-node-to-sexp (mole-parse-string grammar 'xyz "x"))
                     '(x "x")))
      (should (equal (mole-node-to-sexp (mole-parse-string grammar 'xyz "xy"))
                     '(xyz (x "x") (y "y"))))
      (should (equal (mole-node-to-sexp (mole-parse-string grammar 'xyz ""))
                     '(xyz (x) (y)))))
   t))

(defmacro mole-expand-with-debug-on (&rest body)
  "Macroexpand BODY with `mole-build-with-debug' set to t."
  (let ((mole-build-with-debug t))
    (macroexpand-all `(progn ,@body))))

(ert-deftest mole-test-debug-call-stack ()
  "Ensure that when debugging the call stack is set correctly."
  (let ((grammar (mole-expand-with-debug-on
                  (mole-create-grammar
                   (p1 p2)
                   (p2 p3 p4)
                   (p3 p5)
                   (p4 (extern (lambda ()
                                 (should (equal mole-debug-call-stack '(p4 p2 p1)))
                                 'fail)))
                   (p5 (extern (lambda ()
                                 (should (equal mole-debug-call-stack '(p5 p3 p2 p1)))
                                 'fail)))))))
    (mole-parse-string grammar 'p1 "")))

(ert-deftest mole-test-infinite-loop-star ()
  "Ensure infinite loops are detected in debug mode for * nodes."
  (let ((grammar (mole-expand-with-debug-on (mole-create-grammar (p (* (\? "x")))))))
    (let ((err (should-error (mole-parse-string grammar 'p ""))))
      (should (string-match-p "infinite" (cadr err))))))

(ert-deftest mole-test-infinite-loop-plus ()
  "Ensure infinite loops are detected in debug mode for + nodes."
  (let ((grammar (mole-expand-with-debug-on (mole-create-grammar (p (+ (\? "x")))))))
    (let ((err (should-error (mole-parse-string grammar 'p "xxx"))))
      (should (string-match-p "infinite" (cadr err))))))

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
                   (product :pass-thru t number (* (or "*" "/") number))
                   (number :fuse t (+ (char "0-9"))))
                 t)))
    (should (equal (mole-node-to-sexp (mole-parse-string g 'expression "3 \t+ 22 * 6"))
                   '(expression
                     (number "3")
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
                   (b "b" "b"))
                 t)))

    (should (equal (mole-node-to-sexp (mole-parse-string g 'a "a")) '(a "a")))
    (should (equal (mole-node-to-sexp (mole-parse-string g 'a " a")) 'fail))
    (should (equal (mole-node-to-sexp (mole-parse-string g 'b "b  b")) '(b "b" "b")))
    (should (equal (mole-node-to-sexp (mole-parse-string g 'b "bb")) '(b "b" "b")))))

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


;; Left-recursion checks

(ert-deftest mole-count-args-empty ()
  "Check `mole-count-args' with an empty arglist."
  (should (equal (mole-count-args '()) '(0 . 0))))

(ert-deftest mole-count-args-simple ()
  "Check `mole-count-args' with simple arglists."
  (should (equal (mole-count-args '(a)) '(1 . 1)))
  (should (equal (mole-count-args '(a b)) '(2 . 2)))
  (should (equal (mole-count-args '(a b c d e f g h i j)) '(10 . 10))))

(ert-deftest mole-count-args-opt ()
  "Check `mole-count-args' with &optional arguments."
  (should (equal (mole-count-args '(&optional a)) '(0 . 1)))
  (should (equal (mole-count-args '(&optional a b)) '(0 . 2)))
  (should (equal (mole-count-args '(a &optional b c)) '(1 . 3)))
  (should (equal (mole-count-args '(a b &optional c d)) '(2 . 4))))

(ert-deftest mole-count-args-rest ()
  "Check `mole-count-args' with &rest arguments."
  (should (equal (mole-count-args '(&rest a)) '(0 . nil)))
  (should (equal (mole-count-args '(a b &rest c)) '(2 . nil))))

(ert-deftest mole-count-args-opt-rest ()
  "Check `mole-count-args' with &optional and &rest arguments."
  (should (equal (mole-count-args '(&optional a &rest b)) '(0 . nil)))
  (should (equal (mole-count-args '(a &optional b &rest c)) '(1 . nil)))
  (should (equal (mole-count-args '(a b &optional c &rest d)) '(2 . nil))))

(ert-deftest mole-bind-args-empty ()
  "Check `mole-bind-args' with an empty arglist."
  (should (equal (mole-bind-args '() '()) '())))

(ert-deftest mole-bind-args-simple ()
  "Check `mole-bind-args' with simple arglists."
  (should (equal (mole-bind-args '(a) '(1)) '((a 1))))
  (should (equal (mole-bind-args '(a b) '(1 2)) '((a 1) (b 2)))))

(ert-deftest mole-bind-args-opt ()
  "Check `mole-bind-args' with &optional arguments."
  (should (equal (mole-bind-args '(&optional a) '()) '((a nil))))
  (should (equal (mole-bind-args '(&optional a) '(1)) '((a 1))))
  (should (equal (mole-bind-args '(&optional a b) '()) '((a nil) (b nil))))
  (should (equal (mole-bind-args '(&optional a b) '(1)) '((a 1) (b nil))))
  (should (equal (mole-bind-args '(&optional a b) '(1 2)) '((a 1) (b 2))))
  (should (equal (mole-bind-args '(a &optional b c) '(1)) '((a 1) (b nil) (c nil))))
  (should (equal (mole-bind-args '(a &optional b c) '(1 2)) '((a 1) (b 2) (c nil))))
  (should (equal (mole-bind-args '(a &optional b c) '(1 2 3)) '((a 1) (b 2) (c 3))))
  (should (equal (mole-bind-args '(a b &optional c d) '(1 2)) '((a 1) (b 2) (c nil) (d nil)))))

(ert-deftest mole-bind-args-rest ()
  "Check `mole-bind-args' with &rest arguments."
  (should (equal (mole-bind-args '(&rest a) '()) '((a nil))))
  (should (equal (mole-bind-args '(&rest a) '(1 2 3)) '((a (1 2 3)))))
  (should (equal (mole-bind-args '(a b &rest c) '(1 2 3 4)) '((a 1) (b 2) (c (3 4))))))

(ert-deftest mole-bind-args-opt-rest ()
  "Check `mole-bind-args' with &optional and &rest arguments."
  (should (equal (mole-bind-args '(&optional a &rest b) '()) '((a nil) (b nil))))
  (should (equal (mole-bind-args '(&optional a &rest b) '(1)) '((a 1) (b nil))))
  (should (equal (mole-bind-args '(&optional a &rest b) '(1 2)) '((a 1) (b (2)))))
  (should (equal (mole-bind-args '(&optional a &rest b) '(1 2 3)) '((a 1) (b (2 3)))))

  (should (equal (mole-bind-args '(a &optional b &rest c) '(1)) '((a 1) (b nil) (c nil))))
  (should (equal (mole-bind-args '(a &optional b &rest c) '(1 2)) '((a 1) (b 2) (c nil))))
  (should (equal (mole-bind-args '(a &optional b &rest c) '(1 2 3)) '((a 1) (b 2) (c (3)))))
  (should (equal (mole-bind-args '(a &optional b &rest c) '(1 2 3 4)) '((a 1) (b 2) (c (3 4))))))

(ert-deftest mole-lazy ()
  "Check that `mole-lazy' and `mole-unlazy' work as advertised."
  (let* ((a 0) (x (mole-lazy (cl-incf a))))
    (should (equal (list (mole-unlazy x) (mole-unlazy x) a)
                   '(1 1 1)))))

(ert-deftest mole-cycle-detect ()
  "Check that `mole-populate-empty-table' detects a basic cycle."
  (let* ((prods '((a b) (b a)))
         (err (should-error (mole-populate-empty-table prods))))
    (should (string-match-p "recursive" (cadr err)))))

(ert-deftest mole-auto-recursion-detect ()
  "Check that `mole-populate-empty-table' detects left-recursion on itself."
  (let* ((prods '((a "" a)))
         (err (should-error (mole-populate-empty-table prods))))
    (should (string-match-p "recursive" (cadr err)))))

(defun mole-test-empty-table (prods exp)
  "Test the building of the matches-empty-p cache.
PRODS is the alist of productions to consume, and EXP is a list
of (prod-name t) or (prod-name nil) values.  Only productions in
EXP are checked, all others are ignored."
  (let* ((res (mole-populate-empty-table prods)) res-alist)
    (dolist (e exp)
      (push (list (car e) (gethash (car e) res 'not-found)) res-alist))
    (setq res-alist (nreverse res-alist))
    (should (equal res-alist exp))))

(ert-deftest mole-matches-empty-string ()
  (mole-test-empty-table '((a "") (b "x")) '((a t) (b nil))))

(ert-deftest mole-matches-empty-symbol ()
  (mole-test-empty-table '((a b) (b "") (c d) (d "x")) '((a t) (c nil))))

(ert-deftest mole-matches-empty-char ()
  (mole-test-empty-table '((a (char ?a))) '((a nil))))

(ert-deftest mole-matches-empty-char-not ()
  (mole-test-empty-table '((a (char-not ?a))) '((a nil))))

(ert-deftest mole-matches-empty-: ()
  (mole-test-empty-table '((a (: "" "")) (b (: "" "x"))) '((a t) (b nil))))

(ert-deftest mole-matches-empty-or ()
  (mole-test-empty-table '((a (or "xyz" ""))) '((a t))))

(ert-deftest mole-matches-empty-* ()
  (mole-test-empty-table '((a (* "x"))) '((a t))))

(ert-deftest mole-matches-empty-+ ()
  (mole-test-empty-table '((a (+ "x")) (b (+ ""))) '((a nil) (b t))))

(ert-deftest mole-matches-empty-opt ()
  (mole-test-empty-table '((a (\? "x")) (b (opt "x"))) '((a t) (b t))))

(ert-deftest mole-matches-empty-= ()
  (mole-test-empty-table '((a (\?= "x")) (b (= "x"))) '((a t) (b t))))

(ert-deftest mole-matches-empty-! ()
  (mole-test-empty-table '((a (\?! "x")) (b (! "x"))) '((a t) (b t))))

(ert-deftest mole-matches-empty-repetition ()
  (mole-test-empty-table '((a (0 "x")) (b (0 2 "x")) (c (1 "y")) (d (1 3 "y")) (e (1 "")) (f (2 4 "")))
                         '((a t) (b t) (c nil) (d nil) (e t) (f t))))

(ert-deftest mole-matches-empty-lexical ()
  (mole-test-empty-table '((a (lexical "" "")) (b (lexical "" "x"))) '((a t) (b nil))))

(ert-deftest mole-matches-empty-with-context ()
  (mole-test-empty-table '((a (with-context (c 1) "" "")) (b (with-context (c 1) "" "x")))
                         '((a t) (b nil))))

(ert-deftest mole-matches-empty-if-context ()
  (mole-test-empty-table '((a (if-context (c 1) "" "")) (b (if-context (c 1) "" "x")))
                         '((a t) (b nil))))

(ert-deftest mole-matches-empty-extern ()
  (mole-test-empty-table '((a (extern mole-extern-test-fn 1 b))
                           (b (extern (lambda () 'fail))))
                         '((a t) (b t))))

(ert-deftest mole-matches-empty-parametric ()
  (should nil))

(provide 'mole-tests)
;;; mole-test.el ends here
