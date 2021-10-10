(defpackage :cp/inversion-sequence
  (:use :cl :cp/binary-indexed-tree)
  (:export #:perm-to-inv #:inv-to-perm)
  (:documentation "Provides converter between permutation and inversion
sequence."))
(in-package :cp/inversion-sequence)

(deftype uint () '(integer 0 #.most-positive-fixnum))

;; TODO: use biset after resolving compatibility problem
(define-bitree bitree
  :operator #'+
  :identity 0
  :sum-type uint
  :order #'<)

(deftype index () '(integer 0 #.(floor most-positive-fixnum 2)))
(defstruct (itreap (:constructor %make-itreap (value priority &key left right (count 1)))
                   (:copier nil)
                   (:predicate nil)
                   (:conc-name %itreap-))
  (value nil :type fixnum)
  (priority 0 :type (integer 0 #.most-positive-fixnum))
  (count 1 :type index) ; size of (sub)treap
  (left nil :type (or null itreap))
  (right nil :type (or null itreap)))

(declaim (inline itreap-count))
(defun itreap-count (itreap)
  (declare ((or null itreap) itreap))
  (if itreap
      (%itreap-count itreap)
      0))

(declaim (inline update-count))
(defun update-count (itreap)
  (declare (itreap itreap))
  (setf (%itreap-count itreap)
        (+ 1
           (itreap-count (%itreap-left itreap))
           (itreap-count (%itreap-right itreap)))))

(defun itreap-split (itreap index)
  (declare (optimize (speed 3))
           (index index))
  (labels ((recur (itreap ikey)
             (unless itreap
               (return-from itreap-split (values nil nil)))
             (let ((left-count (itreap-count (%itreap-left itreap))))
               (if (<= ikey left-count)
                   (multiple-value-bind (left right)
                       (itreap-split (%itreap-left itreap) ikey)
                     (setf (%itreap-left itreap) right)
                     (update-count itreap)
                     (values left itreap))
                   (multiple-value-bind (left right)
                       (itreap-split (%itreap-right itreap) (- ikey left-count 1))
                     (setf (%itreap-right itreap) left)
                     (update-count itreap)
                     (values itreap right))))))
    (recur itreap index)))

(defun itreap-merge (left right)
  (declare (optimize (speed 3))
           ((or null itreap) left right))
  (cond ((null left) right)
        ((null right) left)
        (t (if (> (%itreap-priority left) (%itreap-priority right))
               (progn
                 (setf (%itreap-right left)
                       (itreap-merge (%itreap-right left) right))
                 (update-count left)
                 left)
               (progn
                 (setf (%itreap-left right)
                       (itreap-merge left (%itreap-left right)))
                 (update-count right)
                 right)))))

(defun itreap-insert (itreap index obj)
  (declare (optimize (speed 3))
           ((or null itreap) itreap)
           (index index))
  (let ((node (%make-itreap obj (random (+ 1 most-positive-fixnum)))))
    (labels ((recur (itreap ikey)
               (declare (index ikey))
               (unless itreap (return-from recur node))
               (if (> (%itreap-priority node) (%itreap-priority itreap))
                   (progn
                     (setf (values (%itreap-left node) (%itreap-right node))
                           (itreap-split itreap ikey))
                     (update-count node)
                     node)
                   (let ((left-count (itreap-count (%itreap-left itreap))))
                     (if (<= ikey left-count)
                         (setf (%itreap-left itreap)
                               (recur (%itreap-left itreap) ikey))
                         (setf (%itreap-right itreap)
                               (recur (%itreap-right itreap) (- ikey left-count 1))))
                     (update-count itreap)
                     itreap))))
      (recur itreap index))))

(declaim (inline itreap-map))
(defun itreap-map (itreap function)
  (declare (function function))
  (labels ((recur (itreap)
             (when itreap
               (recur (%itreap-left itreap))
               (funcall function (%itreap-value itreap))
               (recur (%itreap-right itreap)))))
    (recur itreap)))

(defmethod print-object ((object itreap) stream)
  (print-unreadable-object (object stream :type t)
    (let ((init t))
      (itreap-map object
                  (lambda (x)
                    (if init
                        (setq init nil)
                        (write-char #\  stream))
                    (write x :stream stream))))))

(declaim (inline perm-to-inv))
(defun perm-to-inv (perm)
  (declare (vector perm))
  (let* ((n (length perm))
         (dp (make-array n :element-type 'uint :initial-element 0))
         (res (make-array n :element-type 'uint :initial-element 0)))
    (dotimes (i n)
      (let ((x (aref perm i)))
        (setf (aref res i) (- i (bitree-fold dp x)))
        (bitree-update! dp x 1)))
    res))

(declaim (inline inv-to-perm))
(defun inv-to-perm (inv)
  (declare (vector inv))
  (let ((n (length inv)))
    (when (zerop n)
      (return-from inv-to-perm (make-array 0 :element-type 'uint)))
    (let (itreap)
      (dotimes (pos n)
        (let ((index (- pos (aref inv pos))))
          (setq itreap (itreap-insert itreap index pos))))
      (let ((res (make-array n :element-type 'uint :initial-element 0))
            (x 0))
        (declare (uint x))
        (itreap-map itreap
                    (lambda (pos)
                      (setf (aref res pos) x)
                      (incf x)))
        res))))
