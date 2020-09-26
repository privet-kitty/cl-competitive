(defpackage :cp/circumcenter
  (:use :cl)
  (:export #:calc-circumcenter))
(in-package :cp/circumcenter)

;; http://www.ambrsoft.com/trigocalc/circle3d.htm
(declaim (inline calc-circumcenter))
(defun calc-circumcenter (p1 p2 p3 &optional (eps 1d-8))
  "Returns the center of circumcirlce if it exists, otherwise returns NIL. May
throw a floating point exception if EPS is too small."
  (declare ((real 0) eps))
  (let* ((x1 (realpart p1))
         (y1 (imagpart p1))
         (x2 (realpart p2))
         (y2 (imagpart p2))
         (x3 (realpart p3))
         (y3 (imagpart p3))
         (a (+ (* x1 (- y2 y3))
               (- (* y1 (- x2 x3)))
               (* x2 y3)
               (- (* x3 y2))))
         (b (+ (* (+ (* x1 x1) (* y1 y1)) (- y3 y2))
               (* (+ (* x2 x2) (* y2 y2)) (- y1 y3))
               (* (+ (* x3 x3) (* y3 y3)) (- y2 y1))))
         (c (+ (* (+ (* x1 x1) (* y1 y1)) (- x2 x3))
               (* (+ (* x2 x2) (* y2 y2)) (- x3 x1))
               (* (+ (* x3 x3) (* y3 y3)) (- x1 x2)))))
    (if (< (abs a) eps)
        nil
        (complex (- (/ b (* 2 a)))
                 (- (/ c (* 2 a)))))))
