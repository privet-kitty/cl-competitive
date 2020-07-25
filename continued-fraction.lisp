;;;
;;; Utilities for continued fraction
;;;

;; NOTE: not tested

(defun numerize-cfrac (cfrac)
  "Converts a given continued fraction to a rational number. Returns two values:
numerator and denominator."
  (declare (list cfrac))
  (if (endp cfrac)
      (values 1 0)
      (multiple-value-bind (x y) (calc (cdr cfrac))
        (rotatef x y)
        (values (+ x (* y (car cfrac))) y))))

(defun calc-cfrac-of-sqrt (x)
  "Returns the periodic continued fraction of sqrt(X)."
  (declare ((integer 1 #.most-positive-fixnum) x))
  (let* ((sqrt (isqrt x))
         (p 0)
         (q 1)
         (a sqrt))
    (declare ((integer 0 #.most-positive-fixnum) p q a))
    (cons sqrt
          (unless (= x (* sqrt sqrt))
            (loop do (setq p (- (* a q) p)
                           q (floor (- x (* p p)) q)
                           a (floor (+ sqrt p) q))
                  collect a
                  until (= q 1))))))

(defun solve-pell (d)
  "Returns the minimum potisitive solution of x^2 - Dy^2 = 1. D may not be a
  square number."
  (let* ((cfrac (calc-cfrac-of-sqrt d))
         (len (length cfrac)))
    (multiple-value-bind (num denom)
        (numerize-cfrac (subseq cfrac 0 (- len 1)))
      (if (oddp len)
          (values num denom)
          (values (+ (* num num) (* denom denom d))
                  (* 2 num denom))))))