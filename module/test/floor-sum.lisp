(defpackage :cp/test/floor-sum
  (:use :cl :fiveam :cp/floor-sum)
  (:import-from :cp/test/base #:base-suite))
(in-package :cp/test/floor-sum)
(in-suite base-suite)

(test floor-sum
  (finishes
    (dotimes (n 20)
      (dotimes (slope 20)
        (dotimes (intercept 20)
          (loop for denom from 1 below 20
                do (assert (= (floor-sum n slope intercept denom)
                              (loop for x from 0 below n
                                    sum (floor (+ (* slope x) intercept) denom))))))))))
