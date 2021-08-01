(defpackage :cp/count-set-bit-at
  (:use :cl)
  (:export #:count-set-bit-at))
(in-package :cp/count-set-bit-at)

(declaim (inline count-set-bit-at))
(defun count-set-bit-at (n pos)
  "Returns the number of 1's at POS-th digit of all the integers in {0, 1, ...,
N}."
  (let ((mask (ash 1 (+ pos 1))))
    (+ (* (ash mask -1) (floor n mask))
       (if (zerop (mod n mask))
           0
           (max (- (ash mask -1)
                   (- (* mask (ceiling n mask))
                      (+ n 1)))
                0)))))
