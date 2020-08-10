(defpackage :cp/cumulative-digita-sum
  (:use :cl)
  (:export #:calc-cumulative-digital-sum))
(in-package :cp/cumulative-digita-sum)

(defun calc-cumul-digital-sum (x)
  "Computes DIGITA_SUM(1) + DIGITAL_SUM(2) + ... + DIGITAL_SUM(X)"
  (let* ((seq (nreverse (map '(simple-array (unsigned-byte 8) (*))
                             #'digit-char-p
                             (write-to-string x))))
         (base 1)
         (res 0))
    (loop for pos from 0 below (length seq)
          for max-d = (aref seq pos)
          do (loop for d from 1 to 9
                   do (incf res
                            (cond ((< d max-d)
                                   (* d base (ceiling x (* base 10))))
                                  ((= d max-d)
                                   (* d (+ 1 (mod x base) (* base (floor x (* base 10))))))
                                  (t
                                   (* d base (floor x (* base 10)))))))
             (setq base (* base 10)))
    res))
