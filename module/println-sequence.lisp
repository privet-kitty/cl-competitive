(defpackage :cp/println-sequence
  (:use :cl)
  (:export #:println-sequence))
(in-package :cp/println-sequence)

(declaim (inline println-sequence))
(defun println-sequence (sequence &key (out *standard-output*) (key #'identity))
  (let ((init t))
    (sequence:dosequence (x sequence)
      (if init
          (setq init nil)
          (write-char #\  out))
      (princ (funcall key x) out))
    (terpri out)))
