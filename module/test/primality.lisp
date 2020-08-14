(defpackage :cp/test/primality
  (:use :cl :fiveam :cp/primality)
  (:import-from :cp/test/base #:base-suite))
(in-package :cp/test/primality)
(in-suite base-suite)

(test prime-p
  (let ((state (sb-ext:seed-random-state 0)))
    (finishes
      (loop for x from 0 to 100000
            do (assert (eq (prime-p x) (sb-int:positive-primep x)))))
    (finishes
      (loop repeat 5000
            for x = (+ 4759123141 (* 2 (random 10000000 state)))
            do (assert (eq (prime-p x) (sb-int:positive-primep x)))))))
