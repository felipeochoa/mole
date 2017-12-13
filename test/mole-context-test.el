;;; mole-context-test --- Tests for mole-context  -*- lexical-binding: t -*-
;; Copyright (C) 2017 Felipe Ochoa
;;; Commentary:
;;; Code:

(require 'f)

(load (f-expand "mole-context" (f-parent (f-dirname (f-this-file)))))

(defun mole-context-test-sort (ctx)
  "Return a copy of CTX that has its entries sorted."
  (sort (copy-alist ctx) (lambda (p1 p2)
                           (string< (symbol-name (car p1))
                                    (symbol-name (car p2))))))

(ert-deftest mole-context-compare-eq ()
  (let ((c '((a . 1) (b . 2))))
    (should (mole-context-compare c c))))

(ert-deftest mole-context-compare-equal ()
  (should (mole-context-compare '((a . 1) (b . 2))
                                '((a . 1) (b . 2)))))

(ert-deftest mole-context-compare-out-of-order ()
  (should (mole-context-compare '((a . 1) (b . 2))
                                '((b . 2) (a . 1)))))

(ert-deftest mole-context-compare-different-length ()
  (should-not (mole-context-compare '((a . 1) (b . 2))
                                    '((a . 1) (b . 2) (c . 3)))))

(ert-deftest mole-context-compare-different-keys ()
  (should-not (mole-context-compare '((a . 1) (b . 2))
                                    '((a . 1) (c . 2))))
  (should-not (mole-context-compare '((a . 1) (b . nil))
                                    '((a . 1) (c . 2)))))

(ert-deftest mole-context-compare-different-value ()
  (should-not (mole-context-compare '((a . 1) (b . 2))
                                    '((a . 1) (b . 1))))
  (should-not (mole-context-compare '((a . 1) (b . 2))
                                    '((b . 1) (a . 1))))
  (should-not (mole-context-compare '((a . 1) (b . 2))
                                    '((a . 2) (b . 2))))
  (should-not (mole-context-compare '((a . 1) (b . 2))
                                    '((b . 2) (a . 2)))))

(ert-deftest mole-context-get ()
  (should (eq (mole-context-get '((a . 1) (b . 2)) 'a) 1))
  (should (eq (mole-context-get '((b . 2) (a . 1)) 'b) 2))
  (should (eq (mole-context-get '((a . 1) (b . 2)) 'c) nil)))

(ert-deftest mole-context-set-from-nil ()
  (let (ctx)
    (should (equal (mole-context-test-sort (mole-context-set ctx 'c 3))
                   '((c . 3))))
    (should (eq ctx nil))))

(ert-deftest mole-context-set-new ()
  (let ((ctx '((a . 1) (b . 2))))
    (should (equal (mole-context-test-sort (mole-context-set ctx 'c 3))
                   '((a . 1) (b . 2) (c . 3))))
    (should (equal ctx '((a . 1) (b . 2))))))

(ert-deftest mole-context-set-replace ()
  (let ((ctx '((a . 1) (b . 2))))
    (should (equal (mole-context-test-sort (mole-context-set ctx 'b 3))
                   '((a . 1) (b . 3))))
    (should (equal ctx '((a . 1) (b . 2))))
    (should (equal (mole-context-test-sort (mole-context-set ctx 'a 3))
                   '((a . 3) (b . 2))))
    (should (equal ctx '((a . 1) (b . 2))))))

(provide 'mole-context-test)

;;; mole-context-test.el ends here
