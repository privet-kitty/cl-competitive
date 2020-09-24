(defpackage :cp/test/lagrange-interpolation
  (:use :cl :fiveam :cp/lagrange-interpolation)
  (:import-from :cp/test/base #:base-suite))
(in-package :cp/test/lagrange-interpolation)
(in-suite base-suite)

(defconstant +mod+ #.(+ (expt 10 9) 7))

(test calc-lagrange-base
  (is (equalp #(10 999999985 16)
              (calc-lagrange-base #(1 2 3) #(20 22 32) +mod+)))
  (is (equalp #() (calc-lagrange-base #() #() +mod+)))
  ;; mod 1
  (is (equalp #(0 0 0) (calc-lagrange-base #(1 2 3) #(20 22 32) 1))))

(test lagrange-interpolation
  (is (equalp #(26 999999997 4)
              (lagrange-interpolation #(1 2 3) #(20 22 32) +mod+)))
  (is (equalp #(26 999999997 4 0)
              (lagrange-interpolation #(1 2 3 -1) #(20 22 32 40) +mod+)))
  (is (equalp #() (lagrange-interpolation #() #() +mod+)))
  (is (equalp #(20) (lagrange-interpolation #(1234) #(20) +mod+)))
  ;; mod 1
  (is (equalp #(0 0 0) (lagrange-interpolation #(1 2 3) #(20 22 32) 1))))
