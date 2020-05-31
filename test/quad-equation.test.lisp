(eval-when (:compile-toplevel :load-toplevel :execute)
  (load "test-util")
  (load "../quad-equation.lisp")
  (load "../relative-error.lisp"))

(use-package :test-util)

(defparameter *state* (seed-random-state 123))

(with-test (:name quad-equation)
  (loop for a from -50 to 50
        do (loop for b from -50 to 50
                 do (loop for c from -50 to 50
                          do (multiple-value-bind (res1 res2) (solve-quad-equation a b c)
                               (assert (<= (abs (+ (* (+ (* res1 a) b) res1) c)) 1d-10))
                               (assert (<= (abs (+ (* (+ (* res2 a) b) res2) c)) 1d-10))))))
  (dotimes (_ 10000)
    (let ((a (- (random 2d0 *state*) 1d0))
          (b (- (random 2d0 *state*) 1d0))
          (c (- (random 2d0 *state*) 1d0)))
      (multiple-value-bind (res1 res2) (solve-quad-equation a b c)
        (assert (<= (abs (+ (* (+ (* res1 a) b) res1) c)) 1d-10))
        (assert (<= (abs (+ (* (+ (* res2 a) b) res2) c)) 1d-10))))))
