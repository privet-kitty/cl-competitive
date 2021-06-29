(defpackage :cp/iterset
  (:use :cl)
  (:export #:node-key #:iterset-empty-p #:iterset-first #:iterset-last #:iterset-insert
           #:make-iterset #:iterset-map #:node-next #:node-prev))
(in-package :cp/iterset)

(deftype key-type () 'fixnum)
(declaim (inline order))
(defun order (x y)
  (declare (key-type x y))
  (< x y))

(defstruct (node (:constructor %make-node (key priority &optional left right parent))
                 (:copier nil)
                 (:predicate nil)
                 (:conc-name %node-))
  (key nil :type key-type)
  (priority nil :type (integer 0 #.most-positive-fixnum))
  (left nil :type (or null node))
  (right nil :type (or null node))
  (parent nil :type (or null node)))

(declaim (inline node-key))
(defun node-key (node)
  (%node-key node))

(defstruct (iterset (:constructor make-iterset ())
                    (:copier nil)
                    (:predicate nil)
                    (:conc-name %iterset-))
  (root nil :type (or null node)))

(declaim (inline iterset-empty-p))
(defun iterset-empty-p (iterset)
  (null (%iterset-root iterset)))

(defun node-split (node key)
  "Destructively splits NODE with reference to KEY and returns two trees, the
smaller tree (< KEY) and the larger one (>= KEY)."
  (declare (optimize (speed 3)))
  (labels ((recur (node)
             (cond ((null node)
                    (values nil nil))
                   ((order (%node-key node) key)
                    (multiple-value-bind (left right)
                        (recur (%node-right node))
                      (when left
                        (setf (%node-parent left) node))
                      (setf (%node-right node) left)
                      (values node right)))
                   (t
                    (multiple-value-bind (left right)
                        (recur (%node-left node))
                      (when right
                        (setf (%node-parent right) node))
                      (setf (%node-left node) right)
                      (values left node))))))
    (recur node)))

(defun node-insert (node key)
  (declare (optimize (speed 3))
           ((or null node) node))
  "Destructively inserts KEY into NODE and returns the resultant tree. You
**cannot** rely on the side effect. Use the returned value. The behavior is
undefined when duplicate keys are inserted."
  (let ((new-node (%make-node key (random #.(+ 1 most-positive-fixnum)))))
    (labels ((recur (node)
               (cond ((null node) new-node)
                     ((> (%node-priority new-node) (%node-priority node))
                      (multiple-value-bind (left right)
                          (node-split node key)
                        (when left
                          (setf (%node-parent left) new-node))
                        (when right
                          (setf (%node-parent right) new-node))
                        (setf (%node-left new-node) left
                              (%node-right new-node) right)
                        new-node))
                     ((order key (%node-key node))
                      (let ((left (recur (%node-left node))))
                        (when left
                          (setf (%node-parent left) node))
                        (setf (%node-left node) left))
                      node)
                     (t
                      (let ((right (recur (%node-right node))))
                        (when right
                          (setf (%node-parent right) node))
                        (setf (%node-right node) right))
                      node))))
      (recur node))))

(defun iterset-insert (iterset key)
  "Destructively inserts KEY into ITERSET. You **can** rely on the side
effect. The behavior is undefined when duplicate keys are inserted."
  (setf (%iterset-root iterset)
        (node-insert (%iterset-root iterset) key))
  iterset)

(declaim (inline node-first))
(defun node-first (node)
  (declare (node node))
  (loop while (%node-left node)
        do (setq node (%node-left node)))
  node)

(declaim (inline iterset-first))
(defun iterset-first (iterset)
  "Returns the first **node** of ITERSET."
  (when (%iterset-root iterset)
    (node-first (%iterset-root iterset))))

(defun node-last (node)
  (declare (optimize (speed 3))
           (node node))
  (loop while (%node-right node)
        do (setq node (%node-right node)))
  node)

(declaim (inline iterset-last))
(defun iterset-last (iterset)
  "Returns the last **node** of ITERSET."
  (when (%iterset-root iterset)
    (node-last (%iterset-root iterset))))

(declaim (inline node-map))
(defun node-map (function node)
  "Successively applies FUNCTION to each key of NODE. FUNCTION must take one
argument."
  (declare (function function))
  (labels ((recur (node)
             (when node
               (recur (%node-left node))
               (funcall function (%node-key node))
               (recur (%node-right node)))))
    (recur node)))

(declaim (inline iterset-map))
(defun iterset-map (function iterset)
  "Successively applies FUNCTION to each key of ITERSET. FUNCTION must take one
argument."
  (node-map function (%iterset-root iterset)))

(defmethod print-object ((object node) stream)
  (print-unreadable-object (object stream :type t)
    (let ((init t))
      (node-map (lambda (key)
                  (if init
                      (setf init nil)
                      (write-char #\  stream))
                  (write key :stream stream))
                object))))

(defun node-next (node)
  "Returns the next node of NODE. Returns NIL instead if NODE is the last node."
  (declare (node node))
  (let ((key (%node-key node)))
    (if (%node-right node)
        (node-first (%node-right node))
        (loop while (%node-parent node)
              do (setq node (%node-parent node))
              when (order key (%node-key node))
              do (return node)
              finally (return nil)))))

(defun node-prev (node)
  "Returns the previous node of NODE. Returns NIL instead if NODE is the first
node."
  (declare (node node))
  (let ((key (%node-key node)))
    (if (%node-left node)
        (node-last (%node-left node))
        (loop while (%node-parent node)
              do (setq node (%node-parent node))
              when (order (%node-key node) key)
              do (return node)
              finally (return nil)))))
