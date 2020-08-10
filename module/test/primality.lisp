(eval-when (:compile-toplevel :load-toplevel :execute)
  (load "test-util")
  (load "../primality.lisp"))

(use-package :test-util)

(defparameter *state* (sb-ext:seed-random-state 0))

(with-test (:name prime-p)
  (loop for x from 0 to 100000
        do (assert (eq (prime-p x) (sb-int:positive-primep x))))
  (loop repeat 10000
        for x = (+ 4759123141 (* 2 (random 10000000 *state*)))
        do (assert (eq (prime-p x) (sb-int:positive-primep x)))))
