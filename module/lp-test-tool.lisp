(defpackage :cp/lp-test-tool
  (:use :cl :cp/gaussian)
  (:export #:make-random-instance #:copy))
(in-package :cp/lp-test-tool)

(defun make-random-instance (sigma feasible-p)
  (declare (optimize (speed 3))
           (double-float sigma))
  (let* ((m (round (* 5 (exp (* #.(log 10d0) (random 1d0))))))
         (n (round (* 5 (exp (* #.(log 10d0) (random 1d0))))))
         (as (make-array (list m n) :element-type 'double-float :initial-element 0d0))
         (bs (make-array m :element-type 'double-float :initial-element 0d0))
         (cs (make-array n :element-type 'double-float :initial-element 0d0)))
    (dotimes (i m)
      (dotimes (j n)
        (setf (aref as i j) (* sigma (gaussian)))))
    (dotimes (i m)
      (setf (aref bs i) (* sigma (gaussian))))
    (when feasible-p
      (dotimes (i m)
        (setf (aref bs i) (abs (aref bs i)))))
    (dotimes (j n)
      (setf (aref cs j) (* sigma (gaussian))))
    (values as bs cs)))

(defun copy (a)
  (let ((res (make-array (array-dimensions a) :element-type 'double-float)))
    (replace (sb-ext:array-storage-vector res) (sb-ext:array-storage-vector a))
    res))