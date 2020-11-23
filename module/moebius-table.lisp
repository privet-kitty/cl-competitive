(defpackage :cp/moebius-table
  (:use :cl)
  (:export #:make-moebius-table))
(in-package :cp/moebius-table)

(declaim (ftype (function * (values (simple-array (signed-byte 8) (*))
                                    simple-bit-vector
                                    &optional))
                make-moebius-table))
(defun make-moebius-table (sup)
  "Returns a table of Moebius function. In addition returns a table of primes as
the second value. Time complexity is O(nloglog(n))."
  (declare (optimize (speed 3))
           ((integer 3 (#.array-total-size-limit)) sup))
  (let ((table (make-array sup :element-type 'bit :initial-element 0))
        (result (make-array sup :element-type '(signed-byte 8) :initial-element 1))
        (sup/64 (ceiling sup 64)))
    (setf (aref result 0) 0)
    ;; special treatment for p = 2
    (dotimes (i sup/64)
      (setf (sb-kernel:%vector-raw-bits table i) #xAAAAAAAAAAAAAAAA))
    (setf (sbit table 1) 0
          (sbit table 2) 1)
    (loop for i from 2 below sup by 2
          do (setf (aref result i) (- (aref result i))))
    (loop for i from 4 below sup by 4
          do (setf (aref result i) 0))
    ;; p >= 3
    (loop for p from 3 below sup by 2
          when (= 1 (sbit table p))
          do (let ((sq (* p p)))
               (declare ((mod #.array-total-size-limit) sq))
               (loop for composite from sq below sup by p
                     do (setf (sbit table composite) 0))
               (loop for i from p below sup by p
                     do (setf (aref result i) (- (aref result i))))
               (loop for i from sq below sup by sq
                     do (setf (aref result i) 0))))
    (values result table)))
