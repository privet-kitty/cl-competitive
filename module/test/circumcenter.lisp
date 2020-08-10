(defpackage :cp/test/circumcenter
  (:use :cl :fiveam :cp/circumcenter)
  (:import-from :cp/test/base #:base-suite))
(in-package :cp/test/circumcenter)
(in-suite base-suite)

(test calc-circumcenter
  (is (null (calc-circumcenter #c(0 0) #c(1 1) #c(2 2))))
  (is (not (null (calc-circumcenter #c(0 0) #c(1 0) #c(1 1))))))
