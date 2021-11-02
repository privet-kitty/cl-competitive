(defpackage :cp/test/quad-equation
  (:use :cl :fiveam :cp/quad-equation :cp/relative-error)
  (:import-from :cp/test/base #:base-suite))
(in-package :cp/test/quad-equation)
(in-suite base-suite)

(test quad-equation
  (let ((*test-dribble* nil)
        (state (sb-ext:seed-random-state 0)))
    (finishes
      (loop for a from -30 to 30
            unless (zerop a)
            do (loop for b from -30 to 30
                     do (loop for c from -30 to 30
                              do (multiple-value-bind (res1 res2) (solve-quad-equation a b c)
                                   (assert (<= (abs (+ (* (+ (* res1 a) b) res1) c)) 1d-10))
                                   (assert (<= (abs (+ (* (+ (* res2 a) b) res2) c)) 1d-10)))))))
    (dotimes (_ 10000)
      (let ((a (- (random 2d0 state) 1d0))
            (b (- (random 2d0 state) 1d0))
            (c (- (random 2d0 state) 1d0)))
        (multiple-value-bind (res1 res2) (solve-quad-equation a b c)
          (is (<= (abs (+ (* (+ (* res1 a) b) res1) c)) 1d-8))
          (is (<= (abs (+ (* (+ (* res2 a) b) res2) c)) 1d-8)))))))
