(defpackage :cp/test/perfect-kth-powers
  (:use :cl :fiveam :cp/perfect-kth-powers :cp/linear-sieve :cp/mod-power)
  (:import-from :cp/test/base #:base-suite))
(in-package :cp/test/perfect-kth-powers)
(in-suite base-suite)

(test perfect-kth-powers
  (let ((*test-dribble* nil)
        (minfactor-table (make-minfactor-table 1000)))
    (dotimes (len 20)
      (dotimes (exp 4)
        (loop for mod from 1 to 10
              for vec1 = (make-perfect-kth-powers minfactor-table len exp mod)
              for vec2 = (coerce (loop for x below len
                                       collect (mod-power x exp mod))
                                 '(simple-array (unsigned-byte 31) (*)))
              do (is (equalp vec1 vec2)))))))
