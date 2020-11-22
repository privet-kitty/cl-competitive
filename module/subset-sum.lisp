(defpackage :cp/subset-sum
  (:use :cl)
  (:export #:subset-sum-merge))
(in-package :cp/subset-sum)

(declaim (inline subset-sum-merge))
(defun subset-sum-merge (vector new-value)
  "Merges new item to the set of possible sum of values. VECTOR must be strictly
increasing.

VECTOR := increasing sequence of sum of values"
  (declare (vector vector)
           ((integer 0 #.most-positive-fixnum) new-value))
  (let* ((n (length vector))
         (pos1 0)
         (pos2 0)
         (current-value most-negative-fixnum)
         (res (make-array (* 2 n) :element-type 'fixnum :initial-element 0))
         (end 0))
    (declare ((mod #.array-total-size-limit) pos1 pos2 end)
             (fixnum current-value))
    (loop (when (= pos1 n)
            (loop for pos from pos2 below n
                  for value2 = (+ new-value (aref vector pos))
                  when (< current-value value2)
                  do (setf (aref res end) value2
                           end (+ end 1)
                           current-value value2))
            (return))
          (assert (>= pos1 pos2))
          (let ((value1 (aref vector pos1))
                (value2 (+ new-value (aref vector pos2))))
            (cond ((< value1 value2)
                   (when (> value1 current-value)
                     (setf (aref res end) value1
                           end (+ end 1)
                           current-value value1))
                   (incf pos1))
                  ((> value1 value2)
                   (when (> value2 current-value)
                     (setf (aref res end) value2
                           end (+ end 1)
                           current-value value2))
                   (incf pos2))
                  (t ; value1 = value2
                   (when (> value1 current-value)
                     (setf (aref res end) value1
                           end (+ end 1)
                           current-value value1))
                   (incf pos1)
                   (incf pos2)))))
    (adjust-array res end)))
