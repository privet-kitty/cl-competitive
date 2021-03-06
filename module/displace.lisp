(defpackage :cp/displace
  (:use :cl)
  (:export #:displace))
(in-package :cp/displace)

(declaim (inline displace))
(defun displace (vector &optional (start 0) end)
  "displaced subseq"
  (declare (vector vector)
           ((mod #.array-dimension-limit) start)
           ((or null (mod #.array-dimension-limit)) end))
  (let ((end (or end (length vector))))
    (make-array (- end start)
                :element-type (array-element-type vector)
                :displaced-to vector
                :displaced-index-offset start)))
