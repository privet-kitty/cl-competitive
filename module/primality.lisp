(defpackage :cp/primality
  (:use :cl :cp/tzcount)
  (:export #:prime-p)
  (:documentation "Provides deterministic Miller-Rabin algorithm for primality
test.

This module is tuned for SBCL on x86-64, i.e., here (integer 0
#.MOST-POSITIVE-FIXNUM) is assumed to be (UNSIGNED-BYTE 62)"))
(in-package :cp/primality)

(deftype uint () '(unsigned-byte 62))

(defun %strong-probable-prime-p (n base)
  (declare (optimize (speed 3))
           (uint n base))
  (or (= n base)
      (labels ((mod-power (base power)
                 (declare (uint base power))
                 (loop with res of-type uint = 1
                       while (> power 0)
                       when (oddp power)
                       do (setq res (mod (* res base) n))
                       do (setq base (mod (* base base) n)
                                power (ash power -1))
                       finally (return res))))
        (let* ((d (floor (- n 1) (logand (- n 1) (- 1 n))))
               (y (mod-power base d)))
          (declare (uint y))
          (or (= y 1)
              (= y (- n 1))
              (let ((s (tzcount (- n 1))))
                (loop repeat (- s 1)
                      do (setq y (mod (* y y) n))
                         (when (<= y 1) (return nil))
                         (when (= y (- n 1)) (return t)))))))))

;; https://primes.utm.edu/prove/prove2_3.html
;; TODO: more efficient SPRP
(defun prime-p (n)
  (declare (uint n))
  (cond ((<= n 1) nil)
        ((evenp n) (= n 2))
        ((< n 4759123141)
         (loop for base in '(2 7 61)
               always (%strong-probable-prime-p n base)))
        ((< n 2152302898747)
         (loop for base in '(2 3 5 7 11)
               always (%strong-probable-prime-p n base)))
        ;; NOTE: following branch is not tested
        (t
         (loop for base in '(2 325 9375 28178 450775 9780504 1795265022)
               always (%strong-probable-prime-p n base)))))
