(defpackage :cp/bezout
  (:use :cl :cp/ext-gcd)
  (:export #:solve-bezout #:count-bezout #:%calc-min-factor #:%calc-max-factor))
(in-package :cp/bezout)

(declaim (inline %calc-min-factor))
(defun %calc-min-factor (x alpha)
  "Returns k, so that x+k*alpha is the smallest non-negative number."
  (if (plusp alpha)
      (ceiling (- x) alpha)
      (floor (- x) alpha)))

(declaim (inline %calc-max-factor))
(defun %calc-max-factor (x alpha)
  "Returns k, so that x+k*alpha is the largest non-positive number."
  (if (plusp alpha)
      (floor (- x) alpha)
      (ceiling (- x) alpha)))

(defun solve-bezout (a b c &optional min max)
  "Returns an integer solution of a*x+b*y = c if it exists, otherwise
returns (VALUES NIL NIL).

- If MIN is specified and MAX is null, the returned x is the smallest integer
equal to or larger than MIN.
- If MAX is specified and MIN is null, x is the largest integer equal to or
smaller than MAX.
- If both are specified, x is an integer in [MIN, MAX]. This function returns
NIL when there is no x that satisfies the given condition."
  (declare (fixnum a b c)
           ((or null fixnum) min max))
  (let ((gcd (gcd a b)))
    (if (zerop (mod c gcd))
        (multiple-value-bind (init-x init-y) (ext-gcd a b)
          (let* ((factor (floor c gcd))
                 ;; m*x0 + n*y0 = d
                 (x0 (* init-x factor))
                 (y0 (* init-y factor)))
            (if (and (null min) (null max))
                (values x0 y0)
                (let (;; general solution: x = x0 + kΔx, y = y0 - kΔy
                      (deltax (floor b gcd))
                      (deltay (floor a gcd)))
                  (if min
                      (let* ((k-min (%calc-min-factor (- x0 min) deltax))
                             (x (+ x0 (* k-min deltax)))
                             (y (- y0 (* k-min deltay))))
                        (if (and max (> x max))
                            (values nil nil)
                            (values x y)))
                      (let* ((k-max (%calc-max-factor (- x0 max) deltax))
                             (x (+ x0 (* k-max deltax)))
                             (y (- y0 (* k-max deltay))))
                        (if (<= x max)
                            (values x y)
                            (values nil nil))))))))
        (values nil nil))))

(defun count-bezout (a b c max &optional (include-zero nil))
  "Returns the number of positive (or non-negative, if INCLUDE-ZERO is true)
integers x less than or equal to MAX such that there is a y satisfying
a*x+b*y=c."
  (declare (fixnum a b c)
           ((integer 0 #.most-positive-fixnum) max))
  (if (zerop b)
      (if (zerop a)
          (if (zerop c)
              (+ max (if include-zero 1 0))
              0)
          (multiple-value-bind (x rem) (floor c a)
            (if (zerop rem)
                (if (<= (if include-zero 0 1) x max)
                    1
                    0)
                0)))
      (let ((gcd (gcd a b)))
        (declare (fixnum gcd))
        (if (zerop (mod c gcd))
            (let* ((init-x (ext-gcd a b))
                   (factor (floor c gcd))
                   (x0 (* init-x factor))
                   (delta (floor b gcd))
                   (k-min (%calc-min-factor x0 delta))
                   (rem (+ x0 (* k-min delta)))
                   (mod (abs delta)))
              (+ (floor (+ max (mod (- mod rem) mod))
                        mod)
                 (if (and include-zero (zerop rem))
                     1
                     0)))
            0))))
