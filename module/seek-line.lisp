(defpackage :cp/seek-line
  (:use :cl)
  (:export #:seek-line))
(in-package :cp/seek-line)

(defun seek-line (&optional (stream *standard-input*))
  "Ensures that the next character of STREAM is neither LF nor CR."
  (loop for c = (peek-char nil stream nil #\Space)
        while (or (char= c #\Linefeed)
                  (char= c #\Return))
        do (read-char stream)))
