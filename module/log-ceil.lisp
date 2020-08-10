(defpackage :cp/log-ceil
  (:use :cl)
  (:export #:log2-ceil #:log-ceil))
(in-package :cp/log-ceil)

(declaim (inline log2-ceil))
(defun log2-ceil (x)
  "Rounds up log2(x)."
  (let ((ceil (ceiling x)))
    (declare ((integer 0) ceil))
    (integer-length (- ceil 1))))

(declaim (inline log-ceil))
(defun log-ceil (x base)
  "Rounds up log(x)."
  (declare (real x)
           ((integer 2) base))
  (assert (>= x 0))
  (labels ((%log ()
             (nth-value 0 (ceiling (log x base)))))
    (if (integerp x)
        (let ((y x)
              (result 0))
          (loop (when (zerop y)
                  (return result))
                (multiple-value-bind (quot rem) (floor y base)
                  (unless (zerop rem)
                    (return (%log)))
                  (setq y quot)
                  (incf result))))
        (%log))))

