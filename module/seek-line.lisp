(defpackage :cp/seek-line
  (:use :cl)
  (:export #:seek-line))
(in-package :cp/seek-line)

;; Ensures that the next character of the stream is neither LF nor CR.
(defun seek-line (&optional (stream *standard-input*))
  (loop
    (let ((c (peek-char nil stream nil #\eot)))
      (if (or (char= c #\lf)
	      (char= c #\cr))
          (read-char stream)
          (return)))))
