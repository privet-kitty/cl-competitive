(defpackage :cp/read-digit
  (:use :cl)
  (:export #:read-digit))
(in-package :cp/read-digit)

(declaim (ftype (function * (values (integer 0 9) &optional)) read-digit))
(defun read-digit (&optional (in *standard-input*))
  "Reads a non-negative one-digit integer."
  (macrolet ((%read-byte ()
               `(the (unsigned-byte 8)
                     #+swank (char-code (read-char in nil #\Nul))
                     #-swank (sb-impl::ansi-stream-read-byte in nil #.(char-code #\Nul) nil))))
    (loop (let ((byte (%read-byte)))
            (cond ((<= 48 byte 57)
                   (return (- byte 48)))
                  ((zerop byte) ; #\Nul
                   (error "Read EOF or #\Nul.")))))))
