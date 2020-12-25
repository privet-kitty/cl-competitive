(defpackage :cp/read-schar-until
  (:use :cl :cp/read-schar)
  (:export #:read-schar-until))
(in-package :cp/read-schar-until)

(declaim (inline read-schar-until))
(defun read-schar-until (predicate &optional (stream *standard-input*))
  (loop for c = (read-schar stream)
        until (funcall predicate c)
        finally (return c)))
