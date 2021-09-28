(defpackage :cp/test/primality
  (:use :cl :fiveam :cp/primality :cp/eratosthenes)
  (:import-from :cp/test/base #:base-suite))
(in-package :cp/test/primality)
(in-suite base-suite)

(test prime-p
  (let ((*test-dribble* nil)
        (state (sb-ext:seed-random-state 0))
        (table (make-prime-table 500000)))
    (finishes
      (dotimes (x 500000)
        (assert (eq (if (prime-p x) 1 0) (aref table x)))))
    (loop repeat 5000
          for x = (+ 4759123141 (* 2 (random 10000000 state)))
          do (is (eq (prime-p x) (sb-int:positive-primep x))))))
