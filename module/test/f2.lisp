(defpackage :cp/test/f2
  (:use :cl :fiveam :cp/f2)
  (:import-from :cp/test/base #:base-suite))
(in-package :cp/test/f2)
(in-suite base-suite)

(defun to-bit-matrix (array)
  (destructuring-bind (height width) (array-dimensions array)
    (let ((result (make-array (list (* 64 (ceiling height 64))
                                    (* 64 (ceiling width 64)))
                              :element-type 'bit)))
      (dotimes (i (array-dimension array 0))
        (dotimes (j (array-dimension array 1))
          (setf (aref result i j) (aref array i j))))
      result)))

(defun to-bit-vector (vector)
  (let ((result (make-array (* 64 (ceiling (length vector) 64)) :element-type 'bit)))
    (dotimes (i (length vector))
      (setf (aref result i) (aref vector i)))
    result))

(test f2-gemm
  (is (equalp
       (to-bit-matrix #2a((1 0 0) (0 1 0) (0 0 1)))
       (f2-gemm (to-bit-matrix #2a((0 0 1) (0 1 0) (1 0 0)))
                (to-bit-matrix #2a((0 0 1) (0 1 0) (1 0 0))))))
  (is (equalp
       (to-bit-matrix #2a((1 0 0) (0 0 0) (0 0 0)))
       (f2-gemm (to-bit-matrix #2a((0 0 1) (0 1 0) (1 0 0)))
                (to-bit-matrix #2a((0 0 0) (0 0 0) (1 0 0)))))))

(test f2-gemv
  (is (equalp
       (to-bit-vector #*001)
       (f2-gemv (to-bit-matrix #2a((1 1 1) (0 1 0) (1 0 0)))
                (to-bit-vector #*101))))
  (is (equalp
       (to-bit-vector #*111)
       (f2-gemv (to-bit-matrix #2a((1 0 0) (0 1 0) (0 0 1)))
                (to-bit-vector #*111)))))
