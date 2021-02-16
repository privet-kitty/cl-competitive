(defpackage :cp/round-robin
  (:use :cl)
  (:export #:round-robin-map)
  (:documentation
   "Provides scheduling algorithm (aka circle method) for a round robin tournament.

Reference:
https://en.wikipedia.org/wiki/Round-robin_tournament"))
(in-package :cp/round-robin)

(defun round-robin-map (n function)
  "Maps each list of N-1 matches of a round-robin tournement by N players.

- N must be even.
- FUNCTION takes two arguments: a vector that maps an opponent to each player,
and the number of the round.
- Consequence is undefined when the vector passed FUNCTION is modified.
- Time complexity is O(N^2)."
  (declare (optimize (speed 3))
           ((mod #.array-total-size-limit) n))
  (assert (evenp n))
  (let ((res (make-array n :element-type 'fixnum)))
    (dotimes (round (- n 1))
      (declare ((mod #.array-total-size-limit) round))
      (labels ((calc (x)
                 (declare ((mod #.array-total-size-limit) x))
                 (if (zerop x)
                     0
                     (+ 1 (mod (+ x -1 round) (- n 1))))))
        (dotimes (j (ash n -1))
          (let ((p1 (calc j))
                (p2 (calc (- n j 1))))
            (setf (aref res p1) p2
                  (aref res p2) p1))))
      (funcall function res round))))
