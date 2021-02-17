(defpackage :cp/z-algorithm
  (:use :cl)
  (:export #:make-z-array)
  (:documentation "Provides Z-algorithm."))
(in-package :cp/z-algorithm)

(declaim (inline make-z-array))
(defun make-z-array (vector &key (test #'eql) end)
  "Returns a vector Z of the same length as VECTOR. Z[i] equals to the length of
the longest substring starting at i which coincides with a prefix of VECTOR."
  (declare (vector vector)
           ((or null (mod #.array-total-size-limit)) end))
  (let* ((end (or end (length vector)))
         (base 0)
         (z (make-array end
                        :element-type '(integer 0 #.most-positive-fixnum)
                        :initial-element 0)))
    (declare ((mod #.array-total-size-limit) end base))
    (loop for i from 1 below end
          do (if (< (+ i (aref z (- i base)))
                    (+ base (aref z base)))
                 (setf (aref z i)
                       (aref z (- i base)))
                 (let ((delta (max 0 (- (+ base (aref z base)) i))))
                   (declare ((mod #.array-total-size-limit) delta))
                   (loop while (and (< (+ i delta) end)
                                    (funcall test
                                             (aref vector delta)
                                             (aref vector (+ i delta))))
                         do (incf delta))
                   (setf (aref z i) delta
                         base i))))
    (unless (zerop end)
      (setf (aref z 0) end))
    z))
