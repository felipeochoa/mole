;;; mole-cache-test --- Tests for mole-cache  -*- lexical-binding: t -*-
;; Package-Requires: ((emacs "25.2"))

;;; Commentary:

;;; Code:

(require 'ert)
(require 'ert-x)

(load (let (file-name-handler-alist)
        (expand-file-name
         "../mole-cache"
         (file-name-directory
          (cond                         ; inlined from f-this-file
           (load-in-progress load-file-name)
           ((and (boundp 'byte-compile-current-file) byte-compile-current-file)
            byte-compile-current-file)
           (:else (buffer-file-name)))))))

(defun mole-cache-test-cache (entries)
  "Make a cache and populate it with ENTRIES.
ENTRIES is a list of (pos prod result) lists."
  (dolist (e entries)
    (when (< 2 (cadr e))
      (error "NUM-PRODS is 3, can't use %S as prod" (caddr e))))
  (let ((cache (make-mole-cache :num-prods 3)))
    (dolist (e entries)
      (mole-cache-set cache (car e) (cadr e) (caddr e)))
    cache))

(defun mole-cache-entry-counts (cache)
  "Return a sorted alist of positions to entry counts for CACHE."
  (let (res)
    (maphash
     (lambda (k v)
       (push (cons k (mole-cache-results-vector-num-entries v)) res))
     (mole-cache-table cache))
    (sort res (lambda (x y) (<= (car x) (car y))))))

(ert-deftest mole-cache-basic-get-set ()
  "Ensure the basic cache functionality is working"
  (let ((cache (make-mole-cache :num-prods 3)))
    (should (null (mole-cache-get cache 5 1)))
    (should (eq 'result-1 (mole-cache-set cache 5 1 'result-1)))
    (should (eq 'result-1 (mole-cache-get cache 5 1)))
    (should (eq 'result-1 (mole-cache-get cache 5 1))) ; ensure don't remove

    (should (null (mole-cache-get cache 5 2)))
    (should (null (mole-cache-get cache 2 1)))
    (should (null (mole-cache-get cache 8 1)))

    (should (eq 'result-2 (mole-cache-set cache 5 2 'result-2)))
    (should (eq 'result-2 (mole-cache-get cache 5 2)))
    (should (eq 'result-1 (mole-cache-get cache 5 1))) ; don't clobber

    (should (eq 'result-3 (mole-cache-set cache 8 1 'result-3)))
    (should (eq 'result-3 (mole-cache-get cache 8 1)))
    (should (eq 'result-2 (mole-cache-get cache 5 2))) ; don't clobber
    (should (eq 'result-1 (mole-cache-get cache 5 1)))))

(ert-deftest mole-cache-get-remove ()
  "Ensure the entry is removed when REMOVE is t."
  (let ((cache (mole-cache-test-cache '((5 1 result-1)
                                        (5 2 result-2)
                                        (8 1 result-3)))))
    (should (eq 'result-1 (mole-cache-get cache 5 1 t)))
    (should (null (mole-cache-get cache 5 1)))
    ;; ensure other entries are unaffected
    (should (eq 'result-3 (mole-cache-get cache 8 1)))
    (should (eq 'result-2 (mole-cache-get cache 5 2)))))

(ert-deftest mole-cache-set-entry-cache ()
  "Ensure the number of entries caches are set correctly."
  (let ((cache (make-mole-cache :num-prods 3)))
    (should (= 0 (mole-cache-num-entries cache)))
    (should (equal (mole-cache-entry-counts cache) '()))

    (mole-cache-set cache 5 1 'result-1)
    (should (= 1 (mole-cache-num-entries cache)))
    (should (equal (mole-cache-entry-counts cache) '((5 . 1))))

    (mole-cache-set cache 5 2 'result-2)
    (should (= 2 (mole-cache-num-entries cache)))
    (should (equal (mole-cache-entry-counts cache) '((5 . 2))))

    (mole-cache-set cache 8 1 'result-3)
    (should (= 3 (mole-cache-num-entries cache)))
    (should (equal (mole-cache-entry-counts cache) '((5 . 2) (8 . 1))))))

(ert-deftest mole-cache-get-remove-entry-cache ()
  "Ensure the number of entries caches are updated when removing elements."
  (let ((cache (mole-cache-test-cache '((5 1 result-1)
                                        (5 2 result-2)
                                        (8 1 result-3)))))
    (should (= 3 (mole-cache-num-entries cache)))
    (should (equal (mole-cache-entry-counts cache) '((5 . 2) (8 . 1))))

    (should (eq 'result-1 (mole-cache-get cache 5 1 t)))
    (should (= 2 (mole-cache-num-entries cache)))
    (should (equal (mole-cache-entry-counts cache) '((5 . 1) (8 . 1))))

    (should (eq 'result-3 (mole-cache-get cache 8 1 t)))
    (should (= 1 (mole-cache-num-entries cache)))
    (should (equal (mole-cache-entry-counts cache) '((5 . 1))))

    (should (eq 'result-2 (mole-cache-get cache 5 2 t)))
    (should (= 0 (mole-cache-num-entries cache)))
    (should (equal (mole-cache-entry-counts cache) '()))))

(provide 'mole-cache-tests)
;;; mole-cache-tests.el ends here
