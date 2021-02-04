(defpackage :cp/test/floor-sum
  (:use :cl :fiveam :cp/floor-sum)
  (:import-from :cp/test/base #:base-suite))
(in-package :cp/test/floor-sum)
(in-suite base-suite)

(test floor-sum
  (let ((*test-dribble* nil)
        (cum (make-array 20 :element-type '(unsigned-byte 31) :initial-element 0)))
    (dotimes (slope 20)
      (dotimes (intercept 20)
        (loop for denom from 1 below 20
              do (dotimes (i (- (length cum) 1))
                   (setf (aref cum (+ i 1))
                         (+ (floor (+ (* slope i) intercept) denom)
                            (aref cum i))))
                 (dotimes (i (length cum))
                   (is (= (floor-sum i slope intercept denom)
                          (aref cum i)))))))))
