(defpackage :cp/convolution
  (:use :cl)
  (:export #:convolve))
(in-package :cp/convolution)

(declaim (inline convolve))
(defun convolve (u v &key (op+ #'+) (op* #'*) (identity+ 0))
  "Multiplies two polynomials u(x) and v(x) over an arbitrary ring."
  (declare (vector u v))
  (let* ((deg1 (- (length u) 1))
         (deg2 (- (length v) 1))
         (len (max 0 (+ deg1 deg2 1))))
    (declare ((integer -1 (#.array-total-size-limit)) deg1 deg2 len))
    (when (or (= -1 deg1) (= -1 deg2))
      (return-from convolve (make-array 0 :element-type (array-element-type u))))
    (let ((res (make-array len :element-type (array-element-type u) :initial-element identity+)))
      (declare ((integer -1 (#.array-total-size-limit)) len))
      (dotimes (d len res)
        ;; 0 <= i <= deg1, 0 <= j <= deg2
        (loop with coef = identity+
              for i from (max 0 (- d deg2)) to (min d deg1)
              for j = (- d i)
              do (setq coef (funcall op+ coef (funcall op* (aref u i) (aref v j))))
              finally (setf (aref res d) coef))))))
