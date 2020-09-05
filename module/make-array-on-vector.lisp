(defpackage :cp/make-array-on-vector
  (:use :cl)
  (:export #:make-array-on-vector))
(in-package :cp/make-array-on-vector)

(defun make-array-on-vector (vector dimensions)
  "Returns a multi-dimensional array that uses VECTOR as its storage. The
product of DIMENSIONS must be equal to the length of VECTOR."
  (declare (optimize (speed 3))
           ((simple-array * (*)) vector)
           (list dimensions))
  (assert (= (the fixnum (reduce #'* dimensions)) (length vector)))
  (let* ((array-rank (length dimensions))
         (array (sb-kernel::make-array-header sb-vm::simple-array-widetag array-rank)))
    (declare ((integer 2 (#.array-rank-limit)) array-rank))
    (setf (sb-kernel:%array-available-elements array) (length vector)
          (sb-kernel:%array-displaced-from array) nil
          (sb-kernel:%array-displaced-p array) nil)
    (setf (#.(or (find-symbol "%ARRAY-DATA" :sb-kernel)
                 (find-symbol "%ARRAY-DATA-VECTOR" :sb-kernel))
             array)
          vector)
    (dotimes (axis array-rank)
      (setf (sb-kernel:%array-dimension array axis) (pop dimensions)))
    array))
