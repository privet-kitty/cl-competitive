(defpackage :cp/rho
  (:use :cl :cp/primality)
  (:export #:rfactorize)
  (:documentation "Provides prime factorization by pollard-rho method."))
(in-package :cp/rho)

;; NOTE: not tested

(deftype uint () '(integer 0 #.most-positive-fixnum))

(declaim (ftype (function * (values uint &optional))
                %rho))
(defun %rho (n)
  (declare (optimize (speed 3))
           (uint n))
  (let ((m (ash 1 (integer-length n))))
    (declare (optimize (speed 3) (safety 0)))
    (macrolet ((f (x) `(let ((xx ,x)) (mod (+ c (* xx xx)) n))))
      (loop repeat 100
            for c = (+ 1 (random 100))
            for y of-type uint = 2
            for r of-type uint = 1
            for q of-type uint = 1
            for g of-type uint = 1
            for ys of-type uint = y
            for x of-type uint = y
            do (loop while (= g 1)
                     do (setq x y)
                        (dotimes (_ r)
                          (setq y (f y)))
                        (let ((k 0))
                          (declare (uint k))
                          (loop while (and (< k r) (= g 1))
                                do (setq ys y)
                                   (dotimes (_ (min m (- r k)))
                                     (setq y (f y)
                                           q (mod (* q (abs (- x y))) n)))
                                   (setq g (gcd q n))
                                   (incf k m)))
                        (setq r (ash r 1)))
            when (= g n)
            do (setq g 1)
               (loop while (= g 1)
                     do (setq ys (f ys)
                              g (gcd (abs (- x ys)) n)))
            when (< g n)
            do (return g)
            finally (error "Not found.")))))

(declaim ((simple-array (unsigned-byte 8) (*)) *small-primes*))
(defparameter *small-primes*
  (coerce '(2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71 73 79 83 89 97)
          '(simple-array (unsigned-byte 8) (*))))

(defun rfactorize (x)
  (declare (optimize (speed 3))
           (uint x)
           (inline sort))
  (when (zerop x)
    (return-from rfactorize nil))
  (let (small-pfactors)
    (loop for p of-type (integer 2 100) across *small-primes*
          for exp of-type (integer 0 #.most-positive-fixnum) = 0
          do (loop (multiple-value-bind (quot rem) (floor x p)
                     (unless (zerop rem)
                       (return))
                     (incf exp)
                     (setq x quot)))
             (unless (zerop exp)
               (push (cons p exp) small-pfactors)))
    (let (ps)
      (when (> x 1)
        (let ((stack (list x)))
          (loop
            (unless stack
              (return))
            (let ((x (pop stack)))
              (declare (uint x))
              (if (prime-p x)
                  (push x ps)
                  (let ((factor (%rho x)))
                    (push factor stack)
                    (push (floor x factor) stack))))))
        (setq ps (sort ps (lambda (x y)
                            (declare (uint x y))
                            (> x y)))))
      (let (res)
        (when ps
          (let ((prev (car ps))
                (start 0)
                (pos 0))
            (declare (uint prev start pos))
            (dolist (p ps)
              (declare (uint p))
              (unless (or (zerop pos) (= prev p))
                (push (cons prev (- pos start)) res)
                (setq prev p
                      start pos))
              (incf pos))
            (push (cons prev (- pos start)) res)))
        (nconc (nreverse small-pfactors) res)))))
