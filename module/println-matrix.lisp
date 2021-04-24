(defpackage :cp/println-matrix
  (:use :cl)
  (:export #:println-matrix))
(in-package :cp/println-matrix)

(declaim (inline println-matrix))
(defun println-matrix (array &key (separator #\ ) (key #'identity) (writer #'write) (row-start 0) row-end (col-start 0) col-end)
  "Prints a 2-dimensional array."
  (declare ((array * (* *)) array)
           ((mod #.array-dimension-limit) row-start col-start))
  (let ((row-end (or row-end (array-dimension array 0)))
        (col-end (or col-end (array-dimension array 1))))
    (declare ((mod #.array-dimension-limit) row-end col-end))
    (loop for i from row-start below row-end
          do (loop for j from col-start below col-end
                   unless (= j col-start)
                   do (princ separator)
                   do (funcall writer (funcall key (aref array i j))))
             (terpri))))
