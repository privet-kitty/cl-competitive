(defpackage :cp/anneal
  (:use :cl)
  (:export #:calc-prob))
(in-package :cp/anneal)

(declaim (inline calc-prob))
(defun calc-prob (prev-score new-score temp &optional (maximize t))
  (declare (double-float temp))
  (unless maximize
    (rotatef prev-score new-score))
  (if (>= new-score prev-score)
      1d0
      (let ((x (/ (- new-score prev-score) temp)))
        (exp x))))

(declaim (inline calc-fast-prob))
(defun calc-fast-prob (prev-score new-score temp &optional (maximize t))
  (declare (double-float temp))
  (unless maximize
    (rotatef prev-score new-score))
  (if (>= new-score prev-score)
      1d0
      (let ((x (/ (- new-score prev-score) temp)))
        (max 0d0 (+ (* x (+ (* x (+ (* x (+ (* x (+ (* x #.(float 1/120 1d0)) #.(float 1/24 1d0))) #.(float 1/6 1d0))) #.(float 1/2 1d0))) 1d0)) 1d0)))))
