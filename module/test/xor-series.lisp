(defpackage :cp/test/xor-series
  (:use :cl :fiveam :cp/xor-series)
  (:import-from :cp/test/base #:base-suite))
(in-package :cp/test/xor-series)
(in-suite base-suite)

(test xor-series
  (dotimes (x 50)
    (is (= (xor-series x)
           (loop with res = 0
                 for i from 1 to x
                 do (setq res (logxor res i))
                 finally (return res))))))
