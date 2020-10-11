(defpackage :cp/test/generator
  (:use :cl :fiveam :cp/generator)
  (:import-from :cp/test/base #:base-suite))
(in-package :cp/test/generator)
(in-suite base-suite)

(test calc-generator
  (is (= 3 (calc-generator 167772161)))
  (is (= 3 (calc-generator 469762049)))
  (is (= 11 (calc-generator 754974721)))
  (is (= 3 (calc-generator 998244353)))
  (is (= 500000003 (calc-generator 1000000007 500000003)))
  (is (= 500000006 (calc-generator 1000000007 500000004)))
  (is (= 500000010 (calc-generator 1000000007 500000007)))
  (is (= 500000011 (calc-generator 1000000007 500000011)))
  (is (= 500000012 (calc-generator 1000000007 500000012)))
  (is (= 500000013 (calc-generator 1000000007 500000013))))
