(defpackage :cp/test/two-exponent
  (:use :cl :fiveam :cp/two-exponent)
  (:import-from :cp/test/base #:base-suite))
(in-package :cp/test/two-exponent)
(in-suite base-suite)

(test calc-two-exponent
  (let ((*test-dribble* nil))
    (is (zerop (calc-two-exp 0)))
    (is (zerop (calc-two-exp 0f0)))
    (is (zerop (calc-two-exp 0d0)))
    (loop for denom from -30 to 30
          unless (zerop denom)
          do (loop for num from -30 to 30
                   for x = (/ num denom)
                   for xd = (float x 1d0)
                   for xs = (float x 1f0)
                   do (is (= (calc-two-exp x)
                             (calc-two-exp xd)
                             (calc-two-exp xs)))))))
