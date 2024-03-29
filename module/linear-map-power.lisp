(defpackage :cp/linear-map-power
  (:use :cl :cp/berlekamp-massey :cp/fps :cp/static-mod)
  (:export #:linear-map-power))
(in-package :cp/linear-map-power)

(declaim (inline linear-map-power))
(defun linear-map-power (function vector exp)
  (declare (optimize (speed 3))
           (vector vector))
  (let* ((vector (coerce vector 'mint-vector))
         (dim (length vector))
         (minpoly (nreverse (rminpoly-linear-map function dim +mod+)))
         (coefs (poly-mod-power #(0 1) exp minpoly))
         (res (make-array dim :element-type 'mint :initial-element 0)))
    (declare (mint-vector vector))
    (loop for c across coefs
          do (dotimes (j dim)
               (setf (aref res j)
                     (mod (+ (aref res j) (* (aref vector j) c))
                          +mod+)))
             (setq vector (funcall function vector)))
    res))
