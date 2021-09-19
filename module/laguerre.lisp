(defpackage :cp/laguerre
  (:use :cl :cp/binom-mod-prime)
  (:export #:glaguerre))
(in-package :cp/laguerre)

(defun glaguerre (n alpha)
  "Returns a generalized Laguerre polynomial: L^(\alpha)_n(x).

Reference:
Wen-Kai Shao, Yuan He, Jing Pan. Some identities for the generalized Laguerre
polynomials. 2016."
  (declare ((unsigned-byte 31) n alpha))
  (let ((inv (aref *fact-inv* n))
        (res (make-array (+ n 1) :element-type '(unsigned-byte 31) :initial-element 0)))
    (labels ((mod* (x y)
               (declare ((unsigned-byte 31) x y))
               (mod (* x y) +binom-mod+)))
      (dotimes (k (length res))
        (let ((val (mod* (mod* (mod* (binom n k)
                                     (aref *fact* (- n k)))
                               (binom (+ n alpha) (- n k)))
                         inv)))
          (setf (aref res k)
                (if (evenp k)
                    val
                    (mod (- val) +binom-mod+)))))
      res)))
