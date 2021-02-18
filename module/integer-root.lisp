(defpackage :cp/integer-root
  (:use :cl)
  (:export #:iroot)
  (:documentation
   "Provides computation of the floor of the nth root of an integer. Currently
only deals with positive fixnum.

NOTE: This package depends on SBCL x86-64."))
(in-package :cp/integer-root)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (assert (= most-positive-fixnum (ldb (byte 62 0) -1))))

(defconstant +threshold+ 63 "Nth root of positive fixnum (i.e. less than 2^62)
is always 1 when N is 63 or greater.")

(declaim ((simple-array (unsigned-byte 62) (*)) *supremums*))
(sb-ext:define-load-time-global *supremums*
  (make-array +threshold+ :element-type '(unsigned-byte 62)))

(defun initialize ()
  (loop for exp from 2 below +threshold+
        do (let ((ok 0)
                 (ng 1))
             (loop while (<= (expt ng exp) most-positive-fixnum)
                   do (setq ng (ash ng 1)))
             (loop until (= (- ng ok) 1)
                   for mid = (ash (+ ng ok) -1)
                   when (<= (expt mid exp) most-positive-fixnum)
                   do (setq ok mid)
                   else
                   do (setq ng mid))
             (setf (aref *supremums* exp) ng))))

(initialize)

(declaim (ftype (function * (values (integer 0 #.most-positive-fixnum) &optional))
                %power))
(defun %power (base exp)
  "Is equivalent to CL:EXPT for this purpose."
  (declare (optimize (speed 3) (safety 0))
           ((integer 0 #.most-positive-fixnum) base exp))
  (let ((res 1))
    (declare ((integer 0 #.most-positive-fixnum) res))
    (loop when (oddp exp)
          do (setq res (* res base))
          do (setq exp (ash exp -1))
          until (zerop exp)
          do (setq base (* base base)))
    res))

(declaim (ftype (function * (values (integer 0 #.most-positive-fixnum) &optional))
                iroot))
(defun iroot (x index)
  "Returns the greatest integer less than or equal to the non-negative INDEX-th
root of X."
  (declare (optimize (speed 3))
           ((integer 0 #.most-positive-fixnum) x)
           ((integer 1) index))
  (cond ((zerop x) 0)
        ((>= index +threshold+) 1)
        ((>= index 3)
         (let ((ok 0)
               (ng (aref *supremums* index)))
           (declare ((integer 0 #.most-positive-fixnum) ok ng))
           (loop until (= (- ng ok) 1)
                 for mid = (ash (+ ng ok) -1)
                 when (<= (%power mid index) x)
                 do (setq ok mid)
                 else
                 do (setq ng mid))
           ok))
        ((= index 2) (isqrt x))
        (t x)))
