(eval-when (:compile-toplevel :load-toplevel :execute)
  (load "test-util")
  (load "../f2.lisp"))

(use-package :test-util)

(defun to-bit-matrix (array)
  (destructuring-bind (height width) (array-dimensions array)
    (let ((result (make-array (list (* 64 (ceiling height 64))
                                    (* 64 (ceiling width 64)))
                              :element-type 'bit)))
      (dotimes (i (array-dimension array 0))
        (dotimes (j (array-dimension array 1))
          (setf (aref result i j) (aref array i j))))
      result)))

(with-test (:name f2-gemm)
  (assert (equalp
           (to-bit-matrix #2a((1 0 0) (0 1 0) (0 0 1)))
           (f2-gemm (to-bit-matrix #2a((0 0 1) (0 1 0) (1 0 0)))
                    (to-bit-matrix #2a((0 0 1) (0 1 0) (1 0 0))))))
  (assert (equalp
           (to-bit-matrix #2a((1 0 0) (0 0 0) (0 0 0)))
           (f2-gemm (to-bit-matrix #2a((0 0 1) (0 1 0) (1 0 0)))
                    (to-bit-matrix #2a((0 0 0) (0 0 0) (1 0 0)))))))
