(defpackage :cp/test/inplace-merge
  (:use :cl :fiveam :cp/inplace-merge :cp/run-range)
  (:import-from :cp/test/base #:base-suite))
(in-package :cp/test/inplace-merge)
(in-suite base-suite)

(test inplace-merge-sort/hand
  (is (equalp #() (inplace-merge-sort! (vector) #'<)))
  (is (equalp #(10) (inplace-merge-sort! (vector 10) #'<))))

(test inplace-merge-sort/random
  (let ((*test-dribble* nil)
        (*random-state* (sb-ext:seed-random-state 0)))
    (dotimes (len 100)
      (let ((vector (make-array len :element-type t)))
        (dotimes (i len)
          (setf (aref vector i) (cons (random 20) i)))
        (dotimes (_ 20)
          (let ((start (random (+ 1 len)))
                (end (random (+ 1 len))))
            (when (> start end)
              (rotatef start end))
            (inplace-merge-sort!
             vector
             (lambda (node1 node2) (< (car node1) (car node2))))
            (let ((vector (subseq vector start end)))
              (map-run-range
               (lambda (_ l r)
                 (loop for i from l below (- r 1)
                       for (_ . i1) = (aref vector i)
                       for (__ . i2) = (aref vector (+ i 1))
                       do (is (< i1 i2))))
               vector
               :test (lambda (node1 node2) (eql (car node1) (car node2))))
              (dotimes (i (- (length vector) 1))
                (is (<= (car (aref vector i))
                        (car (aref vector (+ i 1)))))))))))))
