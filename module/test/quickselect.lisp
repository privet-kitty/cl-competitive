(defpackage :cp/test/quickselect
  (:use :cl :fiveam :cp/quickselect)
  (:import-from :cp/test/base #:base-suite))
(in-package :cp/test/quickselect)
(in-suite base-suite)

(test quickselect!
  (declare (notinline quickselect!))
  (let ((vector (make-array 500 :element-type 'fixnum))
        (state (sb-ext:seed-random-state 0)))
    (dotimes (i (length vector))
      (setf (aref vector i) (random 10 state)))
    (let ((sorted (sort (copy-seq vector) #'>)))
      (dotimes (i (length vector))
        (is (= (aref sorted i)
               (quickselect! (copy-seq vector) #'> i))))))
  (let ((vector (vector 1 1 1 1 1 1 1)))
    (dotimes (i (length vector))
      (is (= (aref vector i)
             (quickselect! (copy-seq vector) #'> i)))))
  ;; empty case
  (signals error (quickselect! (vector) #'< 0))
  (is (= 10 (quickselect! (vector 10) #'< 0))))
