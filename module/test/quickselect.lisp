(defpackage :cp/test/quickselect
  (:use :cl :fiveam :cp/quickselect :cp/quicksort)
  (:import-from :cp/test/base #:base-suite))
(in-package :cp/test/quickselect)
(in-suite base-suite)

(test quickselect!
  (let ((*test-dribble* nil)
        (vector (make-array 100 :element-type 'fixnum))
        (state (sb-ext:seed-random-state 0)))
    (dotimes (i (length vector))
      (setf (aref vector i) (random 20 state)))
    (dotimes (l 10)
      (loop for r from 90 to 100
            for tmp = (copy-seq vector)
            do (quicksort! tmp #'> :start l :end r)
               (dotimes (k (- r l))
                 (let* ((vector (copy-seq vector))
                        (res (quickselect! vector #'> k l r)))
                   (is (= (aref tmp (+ l k)) res))
                   (is (loop for i from l below (+ l k)
                             always (>= (aref vector i) (aref vector (+ l k)))))
                   (is (loop for i from (+ l k) below r
                             always (<= (aref vector i) (aref vector (+ l k))))))))))
  (let ((vector (vector 1 1 1 1 1 1 1)))
    (dotimes (i (length vector))
      (is (= (aref vector i)
             (quickselect! (copy-seq vector) #'> i)))))
  ;; empty case
  (signals error (quickselect! (vector) #'< 0))
  (signals error (quickselect! (vector 10) #'< 0 0 0))
  (is (= 10 (quickselect! (vector 10) #'< 0))))
