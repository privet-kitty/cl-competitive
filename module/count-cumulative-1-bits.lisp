(defpackage :cp/count-cumulative-1-bits
  (:use :cl)
  (:export #:count-cumulative-1-bits))
(in-package :cp/count-cumulative-1-bits)

(defun count-cumulative-1-bits (n pos)
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
