(defpackage :cp/pfactors-table
  (:use :cl :cp/tzcount)
  (:export #:make-pfactors-table))
(in-package :cp/pfactors-table)

(declaim (ftype (function * (values (simple-array list (*)) &optional))
                make-pfactors-table))
(defun make-pfactors-table (sup)
  (declare (optimize (speed 3))
           ((mod #.array-dimension-limit) sup))
  (let ((result (make-array sup :element-type 'list :initial-element nil))
        (dp (make-array sup :element-type '(integer 0 #.most-positive-fixnum))))
    (dotimes (i sup)
      (setf (aref dp i) i))
    (loop for x from 2 below sup by 2
          for exp = (tzcount x)
          do (setf (aref dp x) (ash (aref dp x) (- exp)))
             (push (cons 2 exp) (aref result x)))
    (loop for p from 3 below sup by 2
          unless (= 1 (aref dp p))
          do (loop for x from p below sup by p
                   for num of-type (integer 0 #.most-positive-fixnum) = (aref dp x)
                   do (loop for exp of-type (integer 0 #.most-positive-fixnum) from 0
                            do (multiple-value-bind (quot rem) (floor num p)
                                 (unless (zerop rem)
                                   (push (cons p exp) (aref result x))
                                   (setf (aref dp x) num)
                                   (return))
                                 (setq num quot)))))
    (dotimes (x sup)
      (setf (aref result x) (nreverse (aref result x))))
    result))
