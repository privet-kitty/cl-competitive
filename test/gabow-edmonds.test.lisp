(eval-when (:compile-toplevel :load-toplevel :execute)
  (load "test-util")
  (load "../gabow-edmonds.lisp")
  (load "../random-graph.lisp"))

(use-package :test-util)

(defun check-soundness (matching)
  (loop for u below (length matching)
        for v = (aref matching u)
        do (assert (or (= v -1)
                       (= (aref matching v) u)))))

(with-test (:name gabow-edmonds)
  (assert (equalp #() (gabow-edmonds #())))
  (let* ((graph #(() (2 3) (1) (1 4 9) (3 7 8) (6 9) (5 7) (4 6 8) (4 7) (3 5)))
         (res (gabow-edmonds graph)))
    (check-soundness res)
    (assert (= 8 (count-if (lambda (x) (>= x 0)) res)))))

(defparameter *state* (seed-random-state 0))

(with-test (:name gabow-edmonds/random)
  (dotimes (_ 100)
    (let ((graph (make-random-graph (random 100 *state*) (random 1d0 *state*))))
      (check-soundness (gabow-edmonds graph)))))
