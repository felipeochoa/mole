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

DIRTY -- a length-3 vector with [start end delta], indicating
where the buffer is dirty (in old buffer coords) and the net
increase in characters in the dirty area.

CONTEXT-COMPARE-FN -- a 2-argument function used to check whether
a passed in context matches the cached context (t indicates a
match).  When retrieving a cache result, the passed in context is
compared with the stored context using the cache's
CONCTEXT-COMPARE-FN attribute.  If the contexts don't match, the
cache result is not returned.  Defaults to `eq'."
  (table (make-hash-table :test 'eq))
  (num-prods (error "NUM-PRODS is required"))
  (num-entries 0)
  (dirty (vector 0 0 0))
  (context-compare-fn #'eq))

(defsubst mole-cache (num-prods &optional context-compare-fn)
  "Create a new `mole-cache' with NUM-PRODS and CONTEXT-COMPARE-FN."
  (make-mole-cache :num-prods num-prods :context-compare-fn context-compare-fn))

(defun mole-cache-dirty-start (cache)
  "Return the start of the dirty region for CACHE, in old coords."
  (aref (mole-cache-dirty cache) 0))

(gv-define-setter mole-cache-dirty-start (value cache)
  `(setf (aref (mole-cache-dirty ,cache) 0) ,value))

(defun mole-cache-dirty-end (cache)
  "Return the end of the dirty region for CACHE, in old coords."
  (aref (mole-cache-dirty cache) 1))

(gv-define-setter mole-cache-dirty-end (value cache)
  `(setf (aref (mole-cache-dirty ,cache) 1) ,value))

(defun mole-cache-dirty-delta (cache)
  "Return the net increase of the dirty region for CACHE, in old coords."
  (aref (mole-cache-dirty cache) 2))

(gv-define-setter mole-cache-dirty-delta (value cache)
  `(setf (aref (mole-cache-dirty ,cache) 2) ,value))

(defun mole-cache-update-dirty-region (cache beg end delta)
  "Report a new buffer change to CACHE.
\(BEG . END\) is the range of modified characters (in
pre-modification buffer coordinates), DELTA is the net increase
in character count."
  (let* ((vec (mole-cache-dirty cache))
         (old-beg (aref vec 0))
         (old-end (aref vec 1))
         (old-delta (aref vec 2)))
    (setq beg (or (mole-cache-new-to-old cache beg) old-beg))
    (setq end (or (mole-cache-new-to-old cache end) old-end))
    (when (or (zerop old-beg) (< beg old-beg))
      (aset vec 0 beg))
    (when (> end old-end)
      (aset vec 1 end))
    (aset vec 2 (+ old-delta delta))))

(cl-defmacro mole-cache-with-changes (cache (beg-name end-name &optional delta-name) &rest body)
  "Bind CACHE's dirty values to BEG-NAME, END-NAME, DELTA-NAME and execute BODY."
  (declare (indent 2) (debug (form (symbolp symbolp &optional symbolp) &rest form)))
  (let ((cache-name (make-symbol "cache")) (dirty (make-symbol "dirty")))
    `(let* ((,cache-name ,cache)
            (,dirty (mole-cache-dirty ,cache-name))
            (,beg-name (aref ,dirty 0))
            (,end-name (aref ,dirty 1))
            ,@(when delta-name `((,delta-name (aref ,dirty 2)))))
       ,@body)))

(defun mole-cache-clean-p (cache)
  "Return t if the dirty region for CACHE is empty."
  (mole-cache-with-changes cache (beg end)
    (= 0 beg end)))

(defsubst mole-cache-invalid-pos-p (cache pos)
  "Return t if the dirty region for CACHE includes POS.
POS should be in old-buffer coordinates."
  (mole-cache-with-changes cache (beg end)
    (<= beg pos (1- end))))

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

(defun mole-cache-old-to-new* (cache old-pos)
  "Like `mole-cache-old-to-new', but for end-of-range positions.
This function is necessary since end-of-region positions are
considered in the dirty region if the character before them is in
the region.  CACHE and OLD-POS are as in
`mole-cache-old-to-new'."
  (mole-cache-with-changes cache (beg end delta)
    (cond
     ((<= old-pos beg) old-pos)
     ((<= old-pos end) nil)
     (t (+ old-pos delta)))))

(defsubst mole-cache-results-vector-num-entries (vec)
  "Return the number of non-nil entries in VEC.
Relies on the last element of VEC being this value."
  (aref vec (1- (length vec))))

(gv-define-setter mole-cache-results-vector-num-entries (value vec)
  `(setf (aref ,vec (1- (length ,vec))) ,value))

(defun mole-cache-new-vector (cache)
  "Make a new vector for the second level of storage for CACHE."
  (let ((vec (make-vector (1+ (mole-cache-num-prods cache)) nil)))
    (setf (mole-cache-results-vector-num-entries vec) 0)
    vec))

(defsubst mole-cache-compare-contexts (cache c1 c2)
  "Use CACHE's compare-context-fn to compare C1 and C2."
  (funcall (mole-cache-context-compare-fn cache) c1 c2))

(cl-defstruct (mole-cache-result
               (:constructor make-mole-cache-result (pos end context result)))
  "Contains the result of a parse.
In order to enable incremental parsing, parse results have an
`end' field that indicates the furthest point the parser reached
in generating this result. In the case of lookaheads and choice
productions, this point can be past the end of the matched
node."
  pos
  end
  context
  result)

(defun mole-cache-result-valid-p (res cache)
  "Return t if RES is still valid in CACHE."
  (mole-cache-with-changes cache (dirty-beg dirty-end)
      (or (<= (mole-cache-result-end res) dirty-beg)
          (>= (mole-cache-result-pos res) dirty-end))))

(defun mole-cache-set (cache pos end prod-num context res)
  "Store a parse result into CACHE.
POS is the buffer location where parsing is happening.  END is
the last buffer position encountered by the parser.  If the
buffer gets dirty between POS and END, the result will be
invalidated.  PROD-NUM is the numerical index of the production
to check.  CONTEXT is an opaque object used to invalidate cached
entries (see `mole-cache''s CONTEXT-COMPARE-FN for details).  RES
is the result to store, which is returned."
  (cl-assert (mole-cache-clean-p cache))
  (let* ((old-pos (mole-cache-new-to-old cache pos))
         (pos-table (mole-cache-table cache))
         (pos-results (gethash old-pos pos-table)))
    (unless pos-results
      (setq pos-results (puthash old-pos (mole-cache-new-vector cache) pos-table)))
    (unless (aref pos-results prod-num)
      (cl-incf (mole-cache-num-entries cache))
      (cl-incf (mole-cache-results-vector-num-entries pos-results)))
    (setf (aref pos-results prod-num) (make-mole-cache-result pos end context res))
    res))

(defun mole-cache-get (cache pos prod-num context &optional remove)
  "Retrieve a parse result from CACHE or nil if not cached.
POS is the buffer location where parsing is happening.  PROD-NUM
is the numerical index of the production to check.  CONTEXT is an
opaque object used to invalidate cached entries (see
`mole-cache''s CONTEXT-COMPARE-FN for details).  REMOVE, if
non-nil indicates that the entry should be removed from the
cache.

If the parse result is not found, return nil.  If the parse
result is found, the return value is a cons cell (res . hw-mark),
where RES is the parse result and HW-MARK is the highwater-mark
reach while parsing RES (in current buffer coordinates)."
  (when-let (old-pos (mole-cache-new-to-old cache pos))
    ;; OLD-POS could be nil if pos is in the dirty region
    (when-let (pos-results (gethash old-pos (mole-cache-table cache)))
      (when-let (res (aref pos-results prod-num))
        (if (mole-cache-result-valid-p res cache)
            (if (mole-cache-compare-contexts cache (mole-cache-result-context res) context)
                (setq res (cons (mole-cache-result-result res)
                                (mole-cache-old-to-new* cache (mole-cache-result-end res))))
              (setq res nil))
          (setq remove t)
          (setq res nil))
        (when remove
          (cl-decf (mole-cache-num-entries cache))
          (if (= 0 (cl-decf (mole-cache-results-vector-num-entries pos-results)))
              (remhash old-pos (mole-cache-table cache))
            (setf (aref pos-results prod-num) nil)))
        res))))


;;; Cache chaining

(defun mole-cache-transfer-entries (from-cache to-cache)
  "Copy FROM-CACHE's valid entries into TO-CACHE.
TO-CACHE must have an empty dirty region.  If any (pos . prod)
pair is in both caches, the result in TO-CACHE will remain
unchanged.  FROM-CACHE is destructively modified and cannot be
used after this."
  (cl-assert (mole-cache-clean-p to-cache))
  (cl-assert (= (mole-cache-num-prods from-cache)
                (mole-cache-num-prods to-cache)))
  (let ((new-table (mole-cache-table to-cache))
        (num-prods (mole-cache-num-prods from-cache)))
    (maphash
     (lambda (pos results)
       (unless (mole-cache-invalid-pos-p from-cache pos)
         ;; TODO: if TO-CACHE does not have a vector yet, we should
         ;; just re-use the one from FROM-CACHE
         (let* ((new-pos (mole-cache-old-to-new from-cache pos))
                (new-results (or (gethash new-pos new-table)
                                 (puthash new-pos (mole-cache-new-vector to-cache) new-table)))
                (new-entries 0)
                elt)
           (dotimes (i num-prods)
             (unless (aref new-results i)
               (setq elt (aref results i))
               (when (and elt (mole-cache-result-valid-p elt from-cache))
                 (cl-callf2 mole-cache-old-to-new from-cache (mole-cache-result-pos elt))
                 (cl-callf2 mole-cache-old-to-new* from-cache (mole-cache-result-end elt))
                 (aset new-results i elt)
                 (cl-incf new-entries))))
           (cl-incf (mole-cache-results-vector-num-entries new-results) new-entries))))
     (mole-cache-table from-cache))
    ;; ensure any attempts to use FROM-CACHE blow up quickly
    (setf (mole-cache-table from-cache) nil)
    to-cache))

(provide 'mole-cache)

;;; mole-cache.el ends here
