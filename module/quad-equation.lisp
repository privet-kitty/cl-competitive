(defpackage :cp/quad-equation
  (:use :cl)
  (:export #:solve-quad-equation))
(in-package :cp/quad-equation)

(declaim (inline %solve1))
(defun %solve1 (a b c det)
  (let ((result1 (/ (+ (abs b) det) (* 2 a))))
    (if (< b 0)
        (values result1 (/ c (* a result1)))
        (values (- result1) (/ c (* a (- result1)))))))

;; %SOLVE1 is numerically stable but cannot solve x^2 = 0
(declaim (inline %solve2))
(defun %solve2 (a b det)
  (values (/ (+ det b) (* 2 a))
          (/ (- det b) (* 2 a))))

(declaim (inline solve-quad-equation))
(defun solve-quad-equation (a b c)
  "Solves ax^2 + bx + c = 0. A must not be zero."
  (assert (not (zerop a)))
  (let* ((a (float a 1d0))
         (b (float b 1d0))
         (c (float c 1d0))
         (det (sqrt (- (* b b) (* 4 a c)))))
    (labels ((nan-p (x)
               (and (typep x 'double-float)
                    (sb-ext:float-nan-p x))))
      (handler-bind ((arithmetic-error
                       (lambda (_) (declare (ignore _))
                         (return-from solve-quad-equation (%solve2 a b det)))))
        (multiple-value-bind (res1 res2) (%solve1 a b c det)
          (if (or (nan-p res1) (nan-p res2))
              (%solve2 a b det)
              (values res1 res2)))))))
