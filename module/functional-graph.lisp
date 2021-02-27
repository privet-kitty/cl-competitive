(defpackage :cp/functional-graph
  (:use :cl)
  (:export #:make-cycle-info))
(in-package :cp/functional-graph)

(declaim (inline make-cycle-info))
(defun make-cycle-info (f)
  "Takes a vector as a function f: {0, 1, ..., N-1} -> {0, 1, ..., N-1} and
returns two vectors: CYCLE and LENGTH. Time and space complexity is O(N).

CYCLE[v] := the first vertex f(f(...f(v)...)) contained in a cycle.
LENGTH[v] := the length of the cycle that contains V if V is in a cycle, and the
distance to CYCLE[v] otherwise."
  (declare (vector f))
  (let* ((n (length f))
         (tmp (make-array n :element-type 'fixnum :initial-element -1))
         (cycle (make-array n :element-type '(integer 0 #.most-positive-fixnum)
                              :initial-element 0))
         (lengths (make-array n :element-type '(integer 0 #.most-positive-fixnum)
                                :initial-element 0)))
    (dotimes (init n)
      (let ((v init)
            (dist 0))
        (declare ((mod #.array-total-size-limit) v dist))
        (loop
          (unless (= (aref tmp v) -1)
            (if (zerop (aref lengths v))
                ;; seen in this iteration
                (let ((cycle-length (- dist (aref tmp v)))
                      (path-length (aref tmp v)))
                  ;; fill cycle
                  (loop repeat cycle-length
                        do (setf (aref lengths v) cycle-length
                                 (aref cycle v) v
                                 v (aref f v)))
                  ;; fill path
                  (loop with i = init
                        repeat path-length
                        do (setf (aref lengths i) path-length
                                 (aref cycle i) v 
                                 path-length (- path-length 1)
                                 i (aref f i))))
                ;; not seen in this iteration
                (let ((path-length (+ dist
                                      (if (= v (aref cycle v))
                                          0
                                          (aref lengths v)))))
                  ;; fill path
                  (loop with i = init
                        repeat dist
                        do (setf (aref lengths i) path-length
                                 (aref cycle i) (aref cycle v)
                                 path-length (- path-length 1)
                                 i (aref f i)))))
            (return))
          (setf (aref tmp v) dist
                v (aref f v)
                dist (+ dist 1)))))
    (values cycle lengths)))
