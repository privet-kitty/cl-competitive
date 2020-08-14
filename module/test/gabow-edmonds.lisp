(defpackage :cp/test/gabow-edmonds
  (:use :cl :fiveam :cp/gabow-edmonds :cp/random-graph)
  (:import-from :cp/test/base #:base-suite))
(in-package :cp/test/gabow-edmonds)
(in-suite base-suite)

(defun check-soundness (matching)
  (loop for u below (length matching)
        for v = (aref matching u)
        do (assert (or (= v -1)
                       (= (aref matching v) u)))))

(test gabow-edmonds
  (is (equalp #() (find-matching #())))
  (let* ((graph #(() (2 3) (1) (1 4 9) (3 7 8) (6 9) (5 7) (4 6 8) (4 7) (3 5)))
         (res (find-matching graph)))
    (check-soundness res)
    (is (= 8 (count-if (lambda (x) (>= x 0)) res)))))

(defparameter *state* (sb-ext:seed-random-state 0))

(test gabow-edmonds/random
  (dotimes (_ 100)
    (let ((graph (make-random-graph (random 100 *state*) (random 1d0 *state*))))
      (check-soundness (find-matching graph)))))
