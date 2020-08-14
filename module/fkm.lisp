;;;
;;; Generate all the n-bead and k-color necklaces (FKM algorithm)
;;;

(defpackage :cp/fkm
  (:use :cl)
  (:export #:map-necklaces))
(in-package :cp/fkm)

(declaim (inline map-necklaces))
(defun map-necklaces (function length alphabet-size)
  "Example:
> (map-necklaces #'print 4 2)

#(0 0 0 0) 
#(0 0 0 1) 
#(0 0 1 1) 
#(0 1 0 1) 
#(0 1 1 1) 
#(1 1 1 1)
"
  (declare ((integer 1 #.most-positive-fixnum) alphabet-size)
           ((integer 0 #.most-positive-fixnum) length))
  (let ((vec (make-array length
                         :element-type '(integer 0 #.most-positive-fixnum)
                         :initial-element 0))
        (end-pos length))
    (declare ((integer 0 #.most-positive-fixnum) end-pos))
    (funcall function vec)
    (unless (or (= 1 alphabet-size)
                (= 0 length))
      (loop
        (incf (aref vec (- end-pos 1)))
        (loop for j from 0 below (- length end-pos)
              do (setf (aref vec (the fixnum (+ end-pos j)))
                       (aref vec j)))
        (when (zerop (mod length end-pos))
          (funcall function vec))
        (setq end-pos length)
        (loop
          (when (zerop end-pos)
            (return-from map-necklaces))
          (if (= (aref vec (- end-pos 1))
                 (- alphabet-size 1))
              (decf end-pos)
              (return)))))))
