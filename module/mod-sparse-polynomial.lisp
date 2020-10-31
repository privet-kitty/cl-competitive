(defpackage :cp/mod-sparse-polynomial
  (:use :cl :cp/mod-inverse)
  (:export #:poly-sparse-div! #:poly-sparse-mult!)
  (:documentation "Provides poor-man's utilities for operations to dense and
sparse polynomials.

Sparse polynomial is expressed here as a list of (degree
. coefficient). E.g. ((0 . 3) (4 . -5)) expresses 3^5x^4. Note the list must be
in **ascending order** w.r.t. degree."))
(in-package :cp/mod-sparse-polynomial)

(declaim (inline poly-sparse-div!))
(defun poly-sparse-div! (poly sparse-poly modulus)
  "Returnd POLY divided by SPARSE-POLY, regarding polynomial as formal power
series. The result is stored in POLY. Time complexity is O(length(POLY) *
length(SPARSE-POLY))."
  (declare (vector poly)
           ((integer 1 #.most-positive-fixnum) modulus))
  (unless sparse-poly
    (error 'division-by-zero
           :operation #'poly-sparse-div!
           :operands (list poly sparse-poly)))
  (let* ((len (length poly))
         (const-term (car sparse-poly))
         (nonconst-term (cdr sparse-poly))
         (inv (mod-inverse (cdr const-term) modulus)))
    (unless (zerop (car const-term))
      (error "Not implemented. (constant term of ~A is zero)" sparse-poly))
    (dotimes (i len poly)
      (let ((quot-coef (mod (* inv (aref poly i)) modulus)))
        (setf (aref poly i) quot-coef)
        (loop for (deg . coef) in nonconst-term
              while (< (+ i deg) len)
              do (setf (aref poly (+ i deg))
                       (mod (- (aref poly (+ i deg)) (* quot-coef coef))
                            modulus)))))))

(declaim (inline poly-sparse-mult!))
(defun poly-sparse-mult! (poly sparse-poly modulus)
  "Returns POLY multiplied by SPARSE-POLY. The result is stored in POLY. Time
complexity is O(length(POLY) * length(SPARSE-POLY))."
  (let* ((deg1 (- (length poly) 1)))
    (loop for i from deg1 downto 0
          for value = 0
          do (loop for (j . coef) in sparse-poly
                   while (<= j i)
                   when (<= (- i j) deg1)
                   do (setq value (mod (+ value (* coef (aref poly (- i j))))
                                       modulus)))
             (setf (aref poly i) value))
    poly))
