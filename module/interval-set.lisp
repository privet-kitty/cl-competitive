(defpackage :cp/interval-set
  (:use :cl)
  (:export #:interval-set #:interval-set-p #:iset-map #:iset-find #:iset-find>=
           #:iset-insert #:iset-push #:iset-push1 #:iset-delete #:iset-pop #:iset-pop1)
  (:documentation "Provides ordered set of half-open intervals."))
(in-package :cp/interval-set)

;; TODO: more rich operations

(defstruct (interval-set (:constructor %make-interval-set
                             (lkey rkey lnode rnode
                              &aux (priority (random most-positive-fixnum))))
                         (:conc-name %iset-)
                         (:copier nil))
  "This structure maintains an ordered set of half-open intervals with a
balanced binary search tree (aka treap). Every fundamental operation takes
expected O(log(n)) time.

NOTE: For every destructive operation, you cannot rely on a side effect but have
to use a returned value. (i.e. same as destructive operations to cons)
"
  (lkey 0 :type fixnum)
  (rkey 0 :type fixnum)
  (priority 0 :type fixnum)
  (lnode nil :type (or null interval-set))
  (rnode nil :type (or null interval-set)))

(declaim (inline iset-map))
(defun iset-map (function iset)
  "Applies function to each (maximal) interval [L, R) in ISET in ascending
order."
  (labels ((recur (node)
             (when node
               (recur (%iset-lnode node))
               (funcall function (%iset-lkey node) (%iset-rkey node))
               (recur (%iset-rnode node)))))
    (recur iset)))

(defmethod print-object ((object interval-set) stream)
  (print-unreadable-object (object stream :type t)
    (let ((init t))
      (iset-map (lambda (l r)
                  (if init
                      (setq init nil)
                      (write-char #\  stream))
                  (format stream "[~A ~A)" l r))
                object))))

(declaim (ftype (function * (values (or null interval-set) &optional)) %iset-concat))
(defun %iset-concat (left right)
  (declare (optimize (speed 3)))
  (cond ((null left) right)
        ((null right) left)
        ((> (%iset-priority left) (%iset-priority right))
         (setf (%iset-rnode left)
               (%iset-concat (%iset-rnode left) right))
         left)
        (t
         (setf (%iset-lnode right)
               (%iset-concat left (%iset-lnode right)))
         right)))

(declaim (ftype (function * (values (or null interval-set)
                                    (or null interval-set)
                                    &optional))
                %iset-split))
(defun %iset-split (iset lkey)
  "Splits ISET by LKEY. Returns two interval-sets: LEFT and RIGHT. LEFT contains
all the intervals whose left end smaller than LKEY, and RIGHT contains all the
intervals whose left end is equal to or greater than LKEY."
  (declare (optimize (speed 3))
           (fixnum lkey))
  (labels ((recur (node)
             (cond ((null node) (values nil nil))
                   ((< (%iset-lkey node) lkey)
                    (multiple-value-bind (lnode rnode)
                        (recur (%iset-rnode node))
                      (setf (%iset-rnode node) lnode)
                      (values node rnode)))
                   (t
                    (multiple-value-bind (lnode rnode)
                        (recur (%iset-lnode node))
                      (setf (%iset-lnode node) rnode)
                      (values lnode node))))))
    (recur iset)))

(declaim (ftype (function * (values (or null interval-set) &optional)) %iset-insert))
(defun %iset-insert (iset lkey rkey)
  (declare (optimize (speed 3))
           (fixnum lkey rkey))
  (let ((new-node (%make-interval-set lkey rkey nil nil)))
    (labels ((recur (node)
               (cond ((null node) new-node)
                     ((> (%iset-priority new-node) (%iset-priority node))
                      (setf (values (%iset-lnode new-node) (%iset-rnode new-node))
                            (%iset-split node (%iset-lkey new-node)))
                      new-node)
                     (t
                      (if (< (%iset-lkey new-node) (%iset-lkey node))
                          (setf (%iset-lnode node) (recur (%iset-lnode node)))
                          (setf (%iset-rnode node) (recur (%iset-rnode node))))
                      node))))
      (recur iset))))

(declaim (ftype (function * (values fixnum &optional)) %iset-leftmost-key))
(defun %iset-leftmost-key (iset)
  (declare (optimize (speed 3)))
  (if (%iset-lnode iset)
      (%iset-leftmost-key (%iset-lnode iset))
      (%iset-lkey iset)))

(declaim (ftype (function * (values fixnum &optional)) %iset-rightmost-key))
(defun %iset-rightmost-key (iset)
  (declare (optimize (speed 3)))
  (if (%iset-rnode iset)
      (%iset-rightmost-key (%iset-rnode iset))
      (%iset-rkey iset)))

(defun iset-insert (iset lkey rkey)
  "Inserts an interval [LKEY, RKEY) to ISET."
  (declare (optimize (speed 3))
           (fixnum lkey rkey))
  (assert (<= lkey rkey))
  (labels ((lsplit (node)
             (cond ((null node) (values nil nil))
                   ((< (%iset-rkey node) lkey)
                    (multiple-value-bind (lnode rnode)
                        (lsplit (%iset-rnode node))
                      (setf (%iset-rnode node) lnode)
                      (values node rnode)))
                   (t
                    (multiple-value-bind (lnode rnode)
                        (lsplit (%iset-lnode node))
                      (setf (%iset-lnode node) rnode)
                      (values lnode node)))))
           (rsplit (node)
             (cond ((null node) (values nil nil))
                   ((< rkey (%iset-lkey node))
                    (multiple-value-bind (lnode rnode)
                        (rsplit (%iset-lnode node))
                      (setf (%iset-lnode node) rnode)
                      (values lnode node)))
                   (t
                    (multiple-value-bind (lnode rnode)
                        (rsplit (%iset-rnode node))
                      (setf (%iset-rnode node) lnode)
                      (values node rnode))))))
    (if (= lkey rkey)
        iset
        (multiple-value-bind (left tmp) (lsplit iset)
          (multiple-value-bind (mid right) (rsplit tmp)
            (let* ((base (%iset-concat left right))
                   (new-lkey (if mid (min (%iset-leftmost-key mid) lkey) lkey))
                   (new-rkey (if mid (max (%iset-rightmost-key mid) rkey) rkey)))
              (declare (fixnum new-lkey new-rkey))
              (%iset-insert base new-lkey new-rkey)))))))

(defmacro iset-push (lkey rkey iset)
  "PUSH-style macro for ISET-INSERT."
  `(setf ,iset (iset-insert ,iset ,lkey ,rkey)))

(defmacro iset-push1 (key iset)
  "Adds an interval [KEY, KEY+1) to ISET."
  (let ((tmp (gensym)))
    `(let ((,tmp ,key))
       (setf ,iset (iset-insert ,iset ,tmp (+ ,tmp 1))))))

(defun iset-delete (iset lkey rkey)
  "Removes an interval [LKEY, RKEY) from ISET."
  (declare (optimize (speed 3))
           (fixnum lkey rkey))
  (assert (<= lkey rkey))
  (if (= lkey rkey)
      iset
      (labels ((lsplit (node)
                 (cond ((null node) (values nil nil))
                       ((< (%iset-rkey node) lkey)
                        (multiple-value-bind (lnode rnode)
                            (lsplit (%iset-rnode node))
                          (setf (%iset-rnode node) lnode)
                          (values node rnode)))
                       (t
                        (multiple-value-bind (lnode rnode)
                            (lsplit (%iset-lnode node))
                          (setf (%iset-lnode node) rnode)
                          (values lnode node)))))
               (rsplit (node)
                 (cond ((null node) (values nil nil))
                       ((< rkey (%iset-lkey node))
                        (multiple-value-bind (lnode rnode)
                            (rsplit (%iset-lnode node))
                          (setf (%iset-lnode node) rnode)
                          (values lnode node)))
                       (t
                        (multiple-value-bind (lnode rnode)
                            (rsplit (%iset-rnode node))
                          (setf (%iset-rnode node) lnode)
                          (values node rnode))))))
        (multiple-value-bind (left tmp) (lsplit iset)
          (multiple-value-bind (mid right) (rsplit tmp)
            (let ((base (%iset-concat left right))
                  (new-lkey (if mid (min (%iset-leftmost-key mid) lkey) lkey))
                  (new-rkey (if mid (max (%iset-rightmost-key mid) rkey) rkey)))
              (iset-insert (iset-insert base new-lkey lkey)
                           rkey new-rkey)))))))

(defmacro iset-pop (lkey rkey iset)
  "POP-style macro for ISET-INSERT."
  `(setf ,iset (iset-delete ,iset ,lkey ,rkey)))

(defmacro iset-pop1 (key iset)
  "Deletes an interval [KEY, KEY+1) from ISET (if it exists)."
  (let ((tmp (gensym)))
    `(let ((,tmp ,key))
       (setf ,iset (iset-delete ,iset ,tmp (+ ,tmp 1))))))

(declaim (ftype (function * (values (or null fixnum) (or null fixnum) &optional))
                iset-find))
(defun iset-find (iset key)
  "Returns the half-open interval that contains KEY if it exists, otherwise
returns (VALUES NIL NIL)."
  (declare (optimize (speed 3))
           (fixnum key))
  (labels ((recur (node)
             (cond ((null node) (values nil nil))
                   ((< key (%iset-lkey node))
                    (recur (%iset-lnode node)))
                   ((< key (%iset-rkey node))
                    (values (%iset-lkey node) (%iset-rkey node)))
                   (t (recur (%iset-rnode node))))))
    (recur iset)))

(declaim (ftype (function * (values (or null fixnum) (or null fixnum) &optional))
                iset-find>=))
(defun iset-find>= (iset key)
  "Returns the nearest half-open interval that contains KEY or is located on the
larger side of it. Returns (VALUES NIL NIL) if neither of them exist."
  (declare (optimize (speed 3))
           (fixnum key))
  (labels ((recur (node)
             (cond ((null node) (values nil nil))
                   ((<= (%iset-rkey node) key)
                    (recur (%iset-rnode node)))
                   ((<= (%iset-lkey node) key)
                    (values (%iset-lkey node) (%iset-rkey node)))
                   (t (multiple-value-bind (lkey rkey) (recur (%iset-lnode node))
                        (if lkey
                            (values lkey rkey)
                            (values (%iset-lkey node) (%iset-rkey node))))))))
    (recur iset)))
