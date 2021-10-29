(defpackage :cp/test/laguerre
  (:use :cl :fiveam :cp/laguerre :cp/binom-mod-prime)
  (:import-from :cp/test/base #:base-suite))
(in-package :cp/test/laguerre)
(in-suite base-suite)

(test glaguerre
  (is (equalp #(1) (glaguerre 0 0)))
  (is (equalp #(1) (glaguerre 0 1)))
  (is (equalp #(1) (glaguerre 0 2)))
  (is (equalp (vector 1 (- +binom-mod+ 1)) (glaguerre 1 0)))
  (is (equalp (vector 2 (- +binom-mod+ 1)) (glaguerre 1 1)))
  (is (equalp (vector 3 (- +binom-mod+ 1)) (glaguerre 1 2)))
  (is (equalp (vector 1 (- +binom-mod+ 2) (/ (+ +binom-mod+ 1) 2)) (glaguerre 2 0)))
  (is (equalp (vector 3 (- +binom-mod+ 3) (/ (+ +binom-mod+ 1) 2)) (glaguerre 2 1)))
  (is (equalp (vector 6 (- +binom-mod+ 4) (/ (+ +binom-mod+ 1) 2)) (glaguerre 2 2))))
