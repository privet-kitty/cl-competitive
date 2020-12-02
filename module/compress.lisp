(defpackage :cp/compress
  (:use :cl :cp/adjacent-duplicates)
  (:export #:compress!))
(in-package :cp/compress)

(declaim (inline compress!))
(defun compress! (vector order)
  "Does coordinate compression. Destructively modifies VECTOR."
  (declare (inline sort))
  (delete-adjacent-duplicates
   (sort vector order)
   :test (lambda (x y)
           (not (or (funcall order x y) (funcall order y x))))))
