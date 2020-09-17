(defpackage :cp/farey-next
  (:use :cl :cp/bezout)
  (:export #:farey-next #:farey-prev #:phase-next #:phase-prev))
(in-package :cp/farey-next)

(declaim (inline farey-next))
(defun farey-next (num denom max-denom)
  "Returns the next number in a Farey sequence. Returns two values: numerator
and denominator."
  (declare ((integer 1) denom max-denom)
           ((integer 0) num))
  (assert (and (< num denom) (<= denom max-denom)))
  (let* ((gcd (gcd num denom))
         (num (floor num gcd))
         (denom (floor denom gcd)))
    (multiple-value-bind (res-denom res-num) (solve-bezout (- num) denom 1 nil max-denom)
      (values res-num res-denom))))

(declaim (inline farey-prev))
(defun farey-prev (num denom max-denom)
  "Returns the previous number in a Farey sequence. Returns two values: numerator
and denominator."
  (declare ((integer 1) num denom max-denom))
  (assert (and (<= num denom) (<= denom max-denom)))
  (let* ((gcd (gcd num denom))
         (num (floor num gcd))
         (denom (floor denom gcd)))
    (multiple-value-bind (res-denom res-num) (solve-bezout num (- denom) 1 nil max-denom)
      (values res-num res-denom))))

(declaim (ftype (function * (values (or fixnum (complex fixnum)) &optional))
                phase-next phase-prev))
(defun phase-next (point max-denom)
  "Consider all the grid points (x, y) where |x|, |y| <= MAX-DENOM and x and y
are coprime. Also they are sorted anticlockwise by amplitude. This function
returns the `next' point in this sequence."
  (declare ((integer 0 #.most-positive-fixnum) max-denom))
  (assert (not (zerop point)))
  (let* ((x (realpart point))
         (y (imagpart point)))
    (declare (fixnum x y))
    (labels ((calc (x y) ;; for quadrant 1
               (declare ((integer 0 #.most-positive-fixnum) x y))
               (if (> x y)
                   (multiple-value-bind (res-y res-x) (farey-next y x max-denom)
                     (declare ((integer 0 #.most-positive-fixnum) res-x res-y))
                     (complex res-x res-y))
                   (multiple-value-bind (res-x res-y) (farey-prev x y max-denom)
                     (declare ((integer 0 #.most-positive-fixnum) res-x res-y))
                     (complex res-x res-y)))))
      (cond ((and (> x 0) (>= y 0)) ; quadrand 1
             (calc x y))
            ((and (> y 0) (<= x 0)) ; quadrant 2
             (* (calc y (- x)) #c(0 1)))
            ((and (< x 0) (<= y 0)) ; quadrant 3
             (- (calc (- x) (- y))))
            (t ; quadrant 4
             (* (calc (- y) x) #c(0 -1)))))))

(defun phase-prev (point max-denom)
  "Consider all the grid points (x, y) where |x|, |y| <= MAX-DENOM and x and y
are coprime. Also they are sorted anticlockwise by amplitude. This function
returns the `previous' point in this sequence."
  (declare ((integer 0 #.most-positive-fixnum) max-denom))
  (assert (not (zerop point)))
  (let* ((x (realpart point))
         (y (imagpart point)))
    (declare (fixnum x y))
    (labels ((calc (x y) ;; for quadrant 1
               (declare ((integer 0 #.most-positive-fixnum) x y))
               (if (>= x y)
                   (multiple-value-bind (res-y res-x) (farey-prev y x max-denom)
                     (declare ((integer 0 #.most-positive-fixnum) res-x res-y))
                     (complex res-x res-y))
                   (multiple-value-bind (res-x res-y) (farey-next x y max-denom)
                     (declare ((integer 0 #.most-positive-fixnum) res-x res-y))
                     (complex res-x res-y)))))
      (cond ((and (>= x 0) (> y 0))
             (calc x y))
            ((and (>= y 0) (< x 0))
             (* (calc y (- x)) #c(0 1)))
            ((and (<= x 0) (< y 0))
             (- (calc (- x) (- y))))
            (t (* (calc (- y) x) #c(0 -1)))))))
