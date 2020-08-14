(defpackage :cp/test/tzcount
  (:use :cl :fiveam :cp/tzcount)
  (:import-from :cp/test/base #:base-suite))
(in-package :cp/test/tzcount)
(in-suite base-suite)

(test tzcount
  (is (= 0 (tzcount #b1011011)))
  (is (= 1 (tzcount #b1011010)))
  (is (= 3 (tzcount #b0011000)))
  (is (= 0 (tzcount #b1)))
  (is (= -1 (tzcount #b0))))
