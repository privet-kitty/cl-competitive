(defpackage :cp/test/laguerre
  (:use :cl :fiveam :cp/laguerre)
  (:import-from :cp/test/base #:base-suite))
(in-package :cp/test/laguerre)
(in-suite base-suite)

(test glaguerre
  (is (equalp #(1) (glaguerre 0 0)))
  (is (equalp #(1) (glaguerre 0 1)))
  (is (equalp #(1) (glaguerre 0 2)))
  (is (equalp #(1 1000000006) (glaguerre 1 0)))
  (is (equalp #(2 1000000006) (glaguerre 1 1)))
  (is (equalp #(3 1000000006) (glaguerre 1 2)))
  (is (equalp #(1 1000000005 500000004) (glaguerre 2 0)))
  (is (equalp #(3 1000000004 500000004) (glaguerre 2 1)))
  (is (equalp #(6 1000000003 500000004) (glaguerre 2 2))))
