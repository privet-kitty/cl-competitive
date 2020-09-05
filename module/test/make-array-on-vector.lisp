(defpackage :cp/test/make-array-on-vector
  (:use :cl :fiveam :cp/make-array-on-vector)
  (:import-from :cp/test/base #:base-suite))
(in-package :cp/test/make-array-on-vector)
(in-suite base-suite)

(test make-array-on-vector
  (let* ((storage (coerce (loop for i below 12 collect i)
                          '(simple-array (unsigned-byte 16) (*))))
         (array1 (make-array-on-vector storage '(3 4)))
         (array2 (make-array-on-vector storage '(2 6))))
    (is (equalp #2a((0 1 2 3) (4 5 6 7) (8 9 10 11)) array1))
    (is (equalp #2a((0 1 2 3 4 5) (6 7 8 9 10 11)) array2))
    (setf (aref storage 1) 100)
    (is (equalp #2a((0 100 2 3) (4 5 6 7) (8 9 10 11)) array1))
    (is (equalp #2a((0 100 2 3 4 5) (6 7 8 9 10 11)) array2))
    (setf (aref array2 1 0) 600)
    (is (equalp #(0 100 2 3 4 5 600 7 8 9 10 11) storage))
    (is (equalp #2a((0 100 2 3) (4 5 600 7) (8 9 10 11)) array1))))
