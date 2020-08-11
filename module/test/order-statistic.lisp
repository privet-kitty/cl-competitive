(defpackage :cp/test/order-statistic
  (:use :cl :fiveam :cp/order-statistic)
  (:import-from :cp/test/base #:base-suite))
(in-package :cp/test/order-statistic)
(in-suite base-suite)

(test select-ith!
  (declare (notinline select-ith!))
  (let ((vector (make-array 500 :element-type 'fixnum))
        (state (sb-ext:seed-random-state 0)))
    (dotimes (i (length vector))
      (setf (aref vector i) (random 10 state)))
    (let ((sorted (sort (copy-seq vector) #'>)))
      (finishes
        (dotimes (i (length vector))
          (assert (= (aref sorted i)
                     (select-ith! (copy-seq vector) #'> i)))))))
  (let ((vector (vector 1 1 1 1 1 1 1)))
    (finishes
      (dotimes (i (length vector))
        (assert (= (aref vector i)
                   (select-ith! (copy-seq vector) #'> i))))))
  ;; empty case
  (signals error (select-ith! (vector) #'< 0))
  (is (= 10 (select-ith! (vector 10) #'< 0))))
