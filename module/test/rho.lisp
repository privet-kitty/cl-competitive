(defpackage :cp/test/rho
  (:use :cl :fiveam :cp/rho :cp/linear-sieve)
  (:import-from :cp/test/base #:base-suite))
(in-package :cp/test/rho)
(in-suite base-suite)

(test rho
  (let ((mtable (make-minfactor-table 10000))
        (*random-state* (sb-ext:seed-random-state 0))
        (*test-dribble* nil))
    (loop for x below 10000
          do (is (equalp (factorize x mtable)
                         (rfactorize x))))))
