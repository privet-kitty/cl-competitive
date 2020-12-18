(defpackage :cp/sublist
  (:use :cl)
  (:export #:sublist!))
(in-package :cp/sublist)

(defun sublist! (list start &optional end)
  "Returns a contiguous sublist of LIST. This function may destructively modify
LIST. The consequence is undefined when LIST is not proper."
  (declare (optimize (speed 3))
           (list list)
           ((integer 0 #.most-positive-fixnum) start)
           ((or null (integer 0 #.most-positive-fixnum)) end))
  (dotimes (_ start)
    (pop list))
  (cond ((null end) list)
        ((= start end) nil)
        (t (let ((top list))
             (dotimes (_ (- end start 1))
               (pop list))
             (setf (cdr list) nil)
             top))))
