;;; mole.el --- Packrat parser generator  -*- lexical-binding: t -*-

;; Copyright (C) 2017 Felipe Ochoa

;; Author: Felipe Ochoa <felipe@fov.space>
;; URL: https://github.com/felipeochoa/mole/
;; Created: 2 Dec 2017
;; Package-Requires: ((emacs "25.2"))
;; Version: 0.1
;; Keywords: maint

;; This file is not part of GNU Emacs.

;;; Commentary:

;;; Code:

(require 'subr-x)
(require 'cl-lib)

(cl-defstruct (mole-cache)
  "Cache mapping position and production to a parse result.
It maintains a record of buffer changes since the cache was first
populated to allow accessing values even after buffer changes.

Fields:

TABLE -- a hash-table mapping positions to result vectors.
Result vectors have one entry per production with the cached
parse result or nil. They also have an extra item indicating
the number of non-nil entries in the vector.

NUM-PRODS -- how many productions the grammar has.

NUM-ENTRIES -- how many parse results are stored in here.

DIRTY-START -- start (in old buffer coords) of changed buffer area

DIRTY-END -- end (in old buffer coords) of changed buffer area

DIRTY-DELTA -- net increase in characters in the dirty area"
  (table (make-hash-table :test 'eq))
  (num-prods (error "NUM-PRODS is required"))
  (num-entries 0)
  (dirty-start 0)
  (dirty-end 0)
  (dirty-delta 0))

(cl-defmacro mole-cache-with-changes (cache (beg-name end-name delta-name) &rest body)
  "Bind CACHE's dirty values to VARS and execute BODY.
VARS is list of 3 symbols (beg end delta) to bind."
  (declare (indent 2) (debug (form (symbolp symbolp symbolp) &rest form)))
  (let ((cache-name (make-symbol "cache")))
    `(let* ((,cache-name ,cache)
            (,beg-name (mole-cache-dirty-start ,cache-name))
            (,end-name (mole-cache-dirty-end ,cache-name))
            (,delta-name (mole-cache-dirty-delta ,cache-name)))
       ,@body)))

(defun mole-cache-new-to-old (cache new-pos)
  "Get the pre-dirty equivalent for CACHE of NEW-POS.
If NEW-POS is in the dirty region, return nil."
  (mole-cache-with-changes cache (beg end delta)
    (cond
     ((< new-pos beg) new-pos)
     ((< new-pos (+ end delta)) nil)
     (t (- new-pos delta)))))

(defun mole-cache-old-to-new (cache old-pos)
  "Get the post-dirty equivalent for CACHE of OLD-POS.
If OLD-POS is in the dirty region, return nil."
  (mole-cache-with-changes cache (beg end delta)
    (cond
     ((< old-pos beg) old-pos)
     ((< old-pos end) nil)
     (t (+ old-pos delta)))))

(defun mole-cache-new-vector (cache)
  "Make a new vector for the second level of storage for CACHE."
  (let ((vec (make-vector (1+ (mole-cache-num-prods cache)) nil)))
    (setf (mole-cache-results-vector-num-entries vec) 0)
    vec))

(defsubst mole-cache-results-vector-num-entries (vec)
  "Return the number of non-nil entries in VEC.
Relies on the last element of VEC being this value."
  (aref vec (1- (length vec))))

(gv-define-setter mole-cache-results-vector-num-entries (value vec)
  `(setf (aref ,vec (1- (length ,vec))) ,value))

(defun mole-cache-set (cache pos prod-num res)
  "Store a parse result into CACHE.
POS is the buffer location where parsing is happening.  PROD-NUM
is the numerical index of the production to check.  RES is the
result to store, which is returned."
  (mole-cache-with-changes cache (dirty-beg dirty-end dirty-delta)
    (assert (= 0 dirty-beg dirty-end dirty-delta)))
  (let* ((old-pos (mole-cache-new-to-old cache pos))
         (pos-table (mole-cache-table cache))
         (pos-results (gethash old-pos pos-table)))
    (unless pos-results
      (setq pos-results (puthash old-pos (mole-cache-new-vector cache) pos-table)))
    (unless (aref pos-results prod-num)
      (cl-incf (mole-cache-num-entries cache))
      (cl-incf (mole-cache-results-vector-num-entries pos-results)))
    (setf (aref pos-results prod-num) res)))

(defun mole-cache-get (cache pos prod-num &optional remove)
  "Retrieve a parse result from CACHE or nil if not cached.
POS is the buffer location where parsing is happening.  PROD-NUM
is the numerical index of the production to check.  REMOVE, if
non-nil indicates that the entry should be removed from the
cache."
  (when-let (old-pos (mole-cache-new-to-old cache pos))
    ;; OLD-POS could be nil if pos is in the dirty region
    (when-let (pos-results (gethash old-pos (mole-cache-table cache)))
      (let ((res (aref pos-results prod-num)))
        (when (and remove res)
          (cl-decf (mole-cache-num-entries cache))
          (if (= 0 (cl-decf (mole-cache-results-vector-num-entries pos-results)))
              (remhash old-pos (mole-cache-table cache))
            (setf (aref pos-results prod-num) nil)))
        res))))

(provide 'mole-cache)

;;; mole-cache.el ends here
