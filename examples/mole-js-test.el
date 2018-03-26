;;; mole-js-test.el --- Tests for mole-js  -*- lexical-binding: t -*-

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

;;; These tests have been adapted from js2's parser tests

;;; Code:


(require 'mole-js)

(defun mole-js-assert-parse-ok (string)
  "Parse STRING using mole-js and fail if there are any parse errors."
  (let* ((ast (mole-parse-string mole-js-grammar 'program string))
         (errs))
    (mole-visit ast (lambda (n _e)
                      (when (eq (mole-node-clean-name n) 'error)
                        (push n errs))))
    (message "%s" (pp-to-string (mole-node-to-sexp ast)))
    (when errs
      (ert-fail (cons 'parse-errored errs)))))

;;; Basics

(ert-deftest mole-js-variable-assignment ()
  :tags '(mole-js)
  (mole-js-assert-parse-ok "a = 1;"))

(ert-deftest mole-js-empty-object-literal ()
  :tags '(mole-js)
  (mole-js-assert-parse-ok "b = {};"))

(ert-deftest mole-js-empty-array-literal ()
  :tags '(mole-js)
  (mole-js-assert-parse-ok "c = [];"))

(ert-deftest mole-js-array-with-missing-elements ()
  :tags '(mole-js)
  (mole-js-assert-parse-ok "var a = [1, 2, ,];"))

(ert-deftest mole-js-comma-after-regexp ()
  :tags '(mole-js)
  (mole-js-assert-parse-ok "d = /eee/, 42;"))

(ert-deftest mole-js-return-statement ()
  :tags '(mole-js)
  (mole-js-assert-parse-ok "function foo() {\n  return 2;\n}"))

(ert-deftest mole-js-function-statement ()
  :tags '(mole-js)
  (mole-js-assert-parse-ok "function foo() {\n}"))

(ert-deftest mole-js-trailing-comma-in-function-arguments ()
  :tags '(mole-js)
  (mole-js-assert-parse-ok "f(a, b,);"))

(ert-deftest mole-js-function-statement-inside-block ()
  :tags '(mole-js)
  (mole-js-assert-parse-ok "if (true) {\n  function foo() {\n  }\n}"))

;; (js2-deftest-parse function-expression-statements-are-verboten
;;   "function() {}" :syntax-error "(")

;; (js2-deftest-parse member-expr-as-function-name
;;   "function a.b.c[2](x, y) {\n}"
;;   :bind ((js2-allow-member-expr-as-function-name t)))

(ert-deftest mole-js-named-function-expression ()
  :tags '(mole-js)
  (mole-js-assert-parse-ok "a = function b() {};"))

(ert-deftest mole-js-parenthesized-expression ()
  :tags '(mole-js)
  (mole-js-assert-parse-ok "(1 + 2);"))

(ert-deftest mole-js-for-with-in-operator-in-parens ()
  :tags '(mole-js)
  (mole-js-assert-parse-ok "for (var y = (0 in []) in {}) {\n}"))

(ert-deftest mole-js-for-with-in-operator-in-cond ()
  :tags '(mole-js)
  (mole-js-assert-parse-ok "for (var y = 1 ? 0 in [] : false in {}) {\n}"))

(ert-deftest mole-js-let-expression ()
  :tags '(mole-js)
  (mole-js-assert-parse-ok "(let (x = 42) x);"))

(ert-deftest mole-js-let-expression-statement ()
  :tags '(mole-js)
  (mole-js-assert-parse-ok "let (x = 42) x;"))

(ert-deftest mole-js-void ()
  :tags '(mole-js)
  (mole-js-assert-parse-ok "void 0;"))

;;; Callers of `js2-valid-prop-name-token'

(ert-deftest mole-js-parse-property-access-when-not-keyword ()
  :tags '(mole-js)
  (mole-js-assert-parse-ok "A.foo = 3;"))

(ert-deftest mole-js-parse-property-access-when-keyword ()
  :tags '(mole-js)
  (mole-js-assert-parse-ok "A.in = 3;"))

;; (s2-deftest-parse parse-property-access-when-keyword-no-xml
;;   "A.in = 3;"
;;   :bind ((js2-allow-keywords-as-property-names t)
;;          (js2-compiler-xml-available nil)))

(ert-deftest mole-js-parse-object-literal-when-not-keyword ()
  :tags '(mole-js)
  (mole-js-assert-parse-ok "a = {b: 1};"))

(ert-deftest mole-js-parse-object-literal-when-keyword ()
  :tags '(mole-js)
  (mole-js-assert-parse-ok "a = {in: 1};"))

;;; 'of' contextual keyword

(ert-deftest mole-js-parse-legacy-array-comp-loop-with-of ()
  :tags '(mole-js)
  (mole-js-assert-parse-ok "[a for (a of [])];"))

(ert-deftest mole-js-parse-array-comp-loop ()
  :tags '(mole-js)
  (mole-js-assert-parse-ok "[for (a of []) a];"))

(ert-deftest mole-js-parse-for-of ()
  :tags '(mole-js)
  (mole-js-assert-parse-ok "for (var a of []) {\n}"))

(ert-deftest mole-js-of-can-be-name ()
  :tags '(mole-js)
  (mole-js-assert-parse-ok "void of;"))

(ert-deftest mole-js-of-can-be-object-name ()
  :tags '(mole-js)
  (mole-js-assert-parse-ok "of.z;"))

(ert-deftest mole-js-of-can-be-var-name ()
  :tags '(mole-js)
  (mole-js-assert-parse-ok "var of = 3;"))

(ert-deftest mole-js-of-can-be-function-name ()
  :tags '(mole-js)
  (mole-js-assert-parse-ok "function of() {\n}"))






(provide 'mole-js-test)
;;; mole-js-test ends here
