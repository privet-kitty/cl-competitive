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
      (loop for c from 1 below 99
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
            do (loop while (= g 1)
                     do (setq ys (f ys)
                              g (gcd (abs (- x ys)) n)))
            when (< g n)
            do (return g)
            finally (error "Not found.")))))

(defun rfactorize (x)
  (declare (uint x)
           (inline sort))
  (let ((two 0)
        ps)
    (declare (uint two))
    (loop while (evenp x)
          do (setq x (ash x -1))
             (incf two))
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
                        (> x y))))
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
      (when (> two 0)
        (push (cons 2 two) res))
      res)))
