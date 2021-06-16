(defpackage :cp/subset-sum
  (:use :cl)
  (:export #:subset-sum-merge))
(in-package :cp/subset-sum)

(declaim (inline subset-sum-merge))
(defun subset-sum-merge (vector new-value
                         &key
                         (op #'+)
                         (negative-infinity most-negative-fixnum)
                         (drop-duplicate t))
  "Merges new item to the set of possible sum of values. VECTOR must be strictly
increasing.

VECTOR := increasing sequence of sum of values

Example:
\(subset-sum-merge #(0 1 4) 3)
-> #(0 1 3 4 7)"
  (declare (vector vector))
  (let* ((n (length vector))
         (pos1 0)
         (pos2 0)
         (current-value negative-infinity)
         (res (make-array (* 2 n) :element-type t))
         (end 0))
    (declare ((mod #.array-dimension-limit) pos1 pos2 end))
    (labels ((%append (new-value)
               (when (or (not drop-duplicate) (< current-value new-value))
                 (setf (aref res end) new-value
                       current-value new-value
                       end (+ end 1)))))
      (loop (when (= pos1 n)
              (loop for pos from pos2 below n
                    do (%append (funcall op (aref vector pos) new-value)))
              (return))
            (assert (>= pos1 pos2))
            (let ((value1 (aref vector pos1))
                  (value2 (funcall op (aref vector pos2) new-value)))
              (cond ((< value1 value2)
                     (%append value1)
                     (incf pos1))
                    ((> value1 value2)
                     (%append value2)
                     (incf pos2))
                    (t ; value1 = value2
                     (%append value1)
                     (incf pos1)
                     (incf pos2))))))
    (adjust-array res end)))
