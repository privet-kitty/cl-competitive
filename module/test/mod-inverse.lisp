(defpackage :cp/test/mod-inverse
  (:use :cl :fiveam :cp/mod-inverse)
  (:import-from :cp/test/base #:base-suite))
(in-package :cp/test/mod-inverse)
(in-suite base-suite)

(test mod-inverse/random
  (let ((state (sb-ext:seed-random-state 0)))
    (finishes
      (dotimes (i 1000)
        (let ((a (random 100 state))
              (m (+ 2 (random 100 state))))
          (assert (or (/= 1 (gcd a m))
                      (= 1 (mod (* a (mod-inverse a m)) m)))))))))
