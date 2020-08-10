(defpackage :cp/test/ext-gcd
  (:use :cl :fiveam :cp/ext-gcd)
  (:import-from :cp/test/base #:base-suite))
(in-package :cp/test/ext-gcd)
(in-suite base-suite)

(test ext-gcd
  (is (equalp '(-9 47) (multiple-value-list (ext-gcd 240 46))))
  (is (equalp '(9 47) (multiple-value-list (ext-gcd -240 46))))
  (is (equalp '(-9 -47) (multiple-value-list (ext-gcd 240 -46))))
  (is (equalp '(9 -47) (multiple-value-list (ext-gcd -240 -46)))))
