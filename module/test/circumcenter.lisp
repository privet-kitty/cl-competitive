(defpackage :cp/test/circumcenter
  (:use :cl :fiveam :cp/circumcenter)
  (:import-from :cp/test/base #:base-suite))
(in-package :cp/test/circumcenter)
(in-suite base-suite)

(test calc-circumcenter
  (is (null (calc-circumcenter #c(0 0) #c(1 1) #c(2 2))))
  (is (not (null (calc-circumcenter #c(0 0) #c(1 0) #c(1 1)))))
  (is (equal #c(1/2 101/18) (calc-circumcenter #c(0 0) #c(5 9) #c(1 0)))))
