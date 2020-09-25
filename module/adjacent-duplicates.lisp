(defpackage :cp/adjacent-duplicates
  (:use :cl)
  (:export #:delete-adjacent-duplicates))
(in-package :cp/adjacent-duplicates)

(declaim (inline delete-adjacent-duplicates))
(defun delete-adjacent-duplicates (seq &key (test #'eql))
  "Destructively deletes adjacent duplicates of SEQ: e.g. #(1 1 1 2 2 1 3) ->
#(1 2 1 3)"
  (declare (sequence seq)
           (function test))
  (etypecase seq
    (vector
     (if (zerop (length seq))
         seq
         (let ((prev (aref seq 0))
               (end 1))
           (loop for pos from 1 below (length seq)
                 unless (funcall test prev (aref seq pos))
                 do (setf prev (aref seq pos)
                          (aref seq end) (aref seq pos)
                          end (+ 1 end)))
           (if (array-has-fill-pointer-p seq)
               (progn
                 (setf (fill-pointer seq) end)
                 seq)
               (adjust-array seq end)))))
    (list
     (let ((tmp seq))
       (loop while (cdr tmp)
             when (funcall test (first tmp) (second tmp))
             do (setf (cdr tmp) (cddr tmp))
             else do (pop tmp))
       seq))))
