(defpackage :cp/test/bezout
  (:use :cl :fiveam :cp/bezout)
  (:import-from :cp/test/base #:base-suite))
(in-package :cp/test/bezout)
(in-suite base-suite)

(test solve-bezout
  (is (= (%calc-min-factor 8 3) -2))
  (is (= (%calc-min-factor -8 3) 3))
  (is (= (%calc-min-factor 8 -3) 2))
  (is (= (%calc-min-factor -8 -3) -3))
  (is (= (%calc-max-factor 8 3) -3))
  (is (= (%calc-max-factor -8 3) 2))
  (is (= (%calc-max-factor 8 -3) 3))
  (is (= (%calc-max-factor -8 -3) -2)))
