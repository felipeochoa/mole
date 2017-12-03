;;; mole-cache-test --- Tests for mole-cache  -*- lexical-binding: t -*-
;; Package-Requires: ((emacs "25.2"))

;;; Commentary:

;;; Code:

(require 'ert)
(require 'ert-x)
(require 'cl-lib)
(require 'subr-x)

(cl-declaim (optimize (safety 3)))

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
ENTRIES is a list of (pos end prod result) lists."
  (let ((cache (make-mole-cache :num-prods 3)))
    (dolist (e entries)
      (when (< 2 (nth 2 e))
        (error "NUM-PRODS is 3, can't use %S as prod" (caddr e)))
      (apply 'mole-cache-set cache e))
    cache))

(defun mole-cache-entry-counts (cache)
  "Return a sorted alist of positions to entry counts for CACHE."
  (let (res)
    (maphash
     (lambda (k v)
       (push (cons k (mole-cache-results-vector-num-entries v)) res))
     (mole-cache-table cache))
    (sort res (lambda (x y) (<= (car x) (car y))))))

(defun mole-tuple-lte (x y)
  "Lexicographic <= for tuples X and Y."
  (cond
   ((null x) t)
   ((< (car x) (car y)) t)
   ((= (car x) (car y)) (mole-tuple-lte (cdr x) (cdr y)))
   (t nil)))

(defun mole-cache-to-alist (cache)
  "Return a sorted list of (pos end prod res) tuples CACHE."
  (let (res)
    (maphash
     (lambda (pos vec)
       (dotimes (i (1- (length vec)))
         (when-let (elt (aref vec i))
           (push (list pos (mole-cache-result-end elt)
                       i (mole-cache-result-result elt))
                 res))))
     (mole-cache-table cache))
    (sort res 'mole-tuple-lte)))

(ert-deftest mole-cache-basic-get-set ()
  "Ensure the basic cache functionality is working"
  (let ((cache (make-mole-cache :num-prods 3)))
    (should (null (mole-cache-get cache 5 1)))
    (should (eq 'result-1 (mole-cache-set cache 5 5 1 'result-1)))
    (should (eq 'result-1 (mole-cache-get cache 5 1)))
    (should (eq 'result-1 (mole-cache-get cache 5 1))) ; ensure don't remove

    (should (null (mole-cache-get cache 5 2)))
    (should (null (mole-cache-get cache 2 1)))
    (should (null (mole-cache-get cache 8 1)))

    (should (eq 'result-2 (mole-cache-set cache 5 5 2 'result-2)))
    (should (eq 'result-2 (mole-cache-get cache 5 2)))
    (should (eq 'result-1 (mole-cache-get cache 5 1))) ; don't clobber

    (should (eq 'result-3 (mole-cache-set cache 8 8 1 'result-3)))
    (should (eq 'result-3 (mole-cache-get cache 8 1)))
    (should (eq 'result-2 (mole-cache-get cache 5 2))) ; don't clobber
    (should (eq 'result-1 (mole-cache-get cache 5 1)))))

(ert-deftest mole-cache-get-remove ()
  "Ensure the entry is removed when REMOVE is t."
  (let ((cache (mole-cache-test-cache '((5 8 1 result-1)
                                        (5 8 2 result-2)
                                        (8 10 1 result-3)))))
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

    (mole-cache-set cache 5 5 1 'result-1)
    (should (= 1 (mole-cache-num-entries cache)))
    (should (equal (mole-cache-entry-counts cache) '((5 . 1))))

    (mole-cache-set cache 5 6 2 'result-2)
    (should (= 2 (mole-cache-num-entries cache)))
    (should (equal (mole-cache-entry-counts cache) '((5 . 2))))

    (mole-cache-set cache 8 10 1 'result-3)
    (should (= 3 (mole-cache-num-entries cache)))
    (should (equal (mole-cache-entry-counts cache) '((5 . 2) (8 . 1))))))

(ert-deftest mole-cache-get-remove-entry-cache ()
  "Ensure the number of entries caches are updated when removing elements."
  (let ((cache (mole-cache-test-cache '((5 8 1 result-1)
                                        (5 8 2 result-2)
                                        (8 8 1 result-3)))))
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

(ert-deftest mole-cache-get-with-dirty-grew ()
  "Ensure get operations translate positions when buffer is dirty
and larger than originally and invalidate results whose span overlaps
the dirty region."
  (let ((cache (mole-cache-test-cache '((1 2 1 result1) ; pre-gap
                                        (1 3 0 result2) ; pre-gap
                                        (1 7 2 result3) ; covers gap
                                        (2 4 2 result4) ; overlaps gap
                                        (5 9 2 result5) ; starts in gap
                                        (6 9 2 result6) ; after gap
                                        (7 9 0 result7)))))
    (setf (mole-cache-dirty-start cache) 3
          (mole-cache-dirty-end cache) 6
          (mole-cache-dirty-delta cache) 2)
    (should (eq 'result1 (mole-cache-get cache 1 1)))
    (should (eq 'result2 (mole-cache-get cache 1 0)))
    (should-not (mole-cache-get cache 1 2)) ; result3
    (should-not (mole-cache-get cache 2 2)) ; result4
    (should-not (mole-cache-get cache 3 2)) ; result3
    (should-not (mole-cache-get cache 4 2)) ; result4
    (should-not (mole-cache-get cache 5 2)) ; result5
    (should-not (mole-cache-get cache 6 2)) ; for good measure
    (should-not (mole-cache-get cache 7 2)) ; result5
    (should (eq 'result6 (mole-cache-get cache 8 2)))
    (should (eq 'result7 (mole-cache-get cache 9 0)))))

(ert-deftest mole-cache-get-with-dirty-shrank ()
  "Ensure get operations translate positions when buffer is dirty
and smaller than originally and invalidate results whose span
overlaps the dirty region."
  (let ((cache (mole-cache-test-cache '((1 3 1 result1) ; pre-gap
                                        (1 2 0 result2) ; pre-gap
                                        (2 8 2 result3) ; covers gap
                                        (2 4 0 result4) ; overlaps gap
                                        (4 6 0 result5) ; starts in gap
                                        (6 10 1 result6) ; after gap
                                        (10 12 1 result7)))))
    (setf (mole-cache-dirty-start cache) 3
          (mole-cache-dirty-end cache) 6
          (mole-cache-dirty-delta cache) -2)
    (should (eq 'result1 (mole-cache-get cache 1 1)))
    (should (eq 'result2 (mole-cache-get cache 1 0)))
    (should-not (mole-cache-get cache 2 2)) ; result3
    (should-not (mole-cache-get cache 2 0)) ; result4/result5
    (should-not (mole-cache-get cache 3 0)) ; result5
    (should-not (mole-cache-get cache 4 0)) ; result5
    (should (eq 'result6 (mole-cache-get cache 4 1)))
    (should (eq 'result7 (mole-cache-get cache 8 1)))))

(ert-deftest mole-cache-update-dirty-region ()
  (let ((cache (make-mole-cache :num-prods 3)))
    (should (equal [0 0 0] (mole-cache-dirty cache)))

    (mole-cache-update-dirty-region cache 3 3 4)
    (should (equal [3 3 4] (mole-cache-dirty cache)))

    (mole-cache-update-dirty-region cache 3 5 -2)
    (should (equal [3 3 2] (mole-cache-dirty cache)))

    (mole-cache-update-dirty-region cache 5 9 -4)
    (should (equal [3 7 -2] (mole-cache-dirty cache)))

    (mole-cache-update-dirty-region cache 5 6 3)
    (should (equal [3 8 1] (mole-cache-dirty cache)))

    (mole-cache-update-dirty-region cache 2 3 -1)
    (should (equal [2 8 0] (mole-cache-dirty cache)))))


;;; cache chaining

(ert-deftest mole-cache-transfer-entities-both-clean ()
  (let ((from (mole-cache-test-cache '((1 2 0 result0) ; before gap
                                       (2 6 1 result1) ; overlaps gap
                                       (2 8 0 result2) ; covers gap
                                       (2 3 2 result3) ; before gap
                                       (3 6 0 result4) ; starts in gap
                                       (7 9 1 result5) ; after gap
                                       (8 9 0 result6))))
        (to (mole-cache-test-cache '((1 5 2 result7) ; keep & merge row
                                     (3 6 1 result8) ; keep & dont merge row (invalid)
                                     (4 6 0 result9) ; keep & dont merge row (none)
                                     (5 7 1 result10))))) ; keep
    (setf (mole-cache-dirty-start from) 3
          (mole-cache-dirty-end from) 6
          (mole-cache-dirty-delta from) -2)
    (mole-cache-transfer-entries from to)
    (should (equal (mole-cache-to-alist to)
                   '((1 2 0 result0)
                     (1 5 2 result7)
                     (2 3 2 result3)
                     (3 6 1 result8)
                     (4 6 0 result9)
                     (5 7 1 result10)
                     (6 7 0 result6))))))

(provide 'mole-cache-tests)

;;; mole-cache-tests.el ends here
