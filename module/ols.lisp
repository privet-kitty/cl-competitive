(defpackage :cp/ols
  (:use :cl :cp/gauss-jordan)
  (:export #:ols)
  (:documentation "Provides ordinary least squares regression."))
(in-package :cp/ols)

(declaim (ftype (function * (values (or null (simple-array double-float (*)))
                                    &optional))
                ols))
(defun ols (mat y &optional (ridge 0d0))
  "Minimizes |MAT*x - y|^2 in a L2 sense and returns a vector x. RIDGE is a
constant for Ridge regression."
  (declare (optimize (speed 3))
           ((simple-array double-float (* *)) mat)
           ((simple-array double-float (*)) y)
           (double-float ridge))
  (destructuring-bind (m n) (array-dimensions mat)
    (declare ((mod #.array-dimension-limit) m n))
    (let ((lhs (make-array (list n n) :element-type 'double-float))
          (rhs (make-array n :element-type 'double-float)))
      (dotimes (i n)
        (loop for j from i below n
              for value of-type double-float = 0d0
              do (dotimes (k m)
                   (incf value (* (aref mat k i) (aref mat k j))))
                 (setf (aref lhs i j) value
                       (aref lhs j i) value)))
      (unless (zerop ridge)
        (dotimes (i n)
          (incf (aref lhs i i) ridge)))
      (dotimes (j n)
        (let ((value 0d0))
          (declare (double-float value))
          (dotimes (i m)
            (incf value (* (aref mat i j) (aref y i))))
          (setf (aref rhs j) value)))
      (nth-value 0 (solve-linear-system lhs rhs)))))
