(defpackage :cp/monotone-minima
  (:use :cl)
  (:export #:monotone-minima))
(in-package :cp/monotone-minima)

;; not tested

(declaim (inline monotone-minima))
(defun monotone-minima (function x1 y1 x2 y2 &key (order #'<))
  "Takes a monotone two-variable function and returns a vector such that for all
x in [x1, x2) f(x, y) takes a minimum at y = vector[x - X1]. If order is #'>,
this function finds maxima instead."
  (declare (fixnum x1 y1 x2 y2))
  (let ((result (make-array (- x2 x1) :element-type '(integer 0 #.most-positive-fixnum)))
        (offset x1))
    (labels ((recur (x1 y1 x2 y2)
               (declare (fixnum x1 y1 x2 y2))
               (when (< x1 x2)
                 (let ((mid-x (ash (+ x1 x2) -1))
                       min-value
                       min-index)
                   (declare (fixnum mid-x))
                   (loop for y from y1 below y2
                         for value = (funcall function mid-x y)
                         when (or (null min-value)
                                  (funcall order value min-value))
                         do (setq min-value value
                                  min-index y))
                   (setf (aref result (- mid-x offset)) min-index)
                   (recur x1 y1 mid-x (+ min-index 1))
                   (recur (+ mid-x 1) min-index x2 y2)))))
      (recur x1 y1 x2 y2)
      result)))
