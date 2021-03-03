(defpackage :cp/integer-root
  (:use :cl)
  (:export #:iroot)
  (:documentation
   "Provides computation of the floor of the nth root of an integer."))
(in-package :cp/integer-root)

;; This value is set for SBCL x86-64
(defconstant +bit-width+ 62)
(deftype uint () '(unsigned-byte #.+bit-width+))

(declaim ((simple-array uint (*)) *supremums*))
(defparameter *supremums*
  (make-array (+ 1 +bit-width+) :element-type 'uint))

(defun initialize ()
  (let ((most-uint (ldb (byte +bit-width+ 0) -1)))
    (loop for exp from 2 to +bit-width+
          do (let ((ok 0)
                   (ng 1))
               (loop while (<= (expt ng exp) most-uint)
                     do (setq ng (ash ng 1)))
               (loop until (= (- ng ok) 1)
                     for mid = (ash (+ ng ok) -1)
                     when (<= (expt mid exp) most-uint)
                     do (setq ok mid)
                     else
                     do (setq ng mid))
               (setf (aref *supremums* exp) ng)))))

(initialize)

(declaim (inline %power)
         (ftype (function * (values uint &optional)) %power))
(defun %power (base exp)
  "Is equivalent to CL:EXPT for this purpose."
  (declare (uint base exp))
  (let ((res 1))
    (declare (uint res))
    (loop when (oddp exp)
          do (setq res (* res base))
          do (setq exp (ash exp -1))
          until (zerop exp)
          do (setq base (* base base)))
    res))

(declaim (ftype (function * (values uint &optional)) iroot))
(defun iroot (x index)
  "Returns the greatest integer less than or equal to the non-negative INDEX-th
root of X."
  (declare (optimize (speed 3))
           (uint x)
           ((integer 1) index))
  (locally (declare (optimize (safety 0)))
    (cond ((zerop x) 0)
          ((> index +bit-width+) 1)
          ((>= index 3)
           (let ((ok 0)
                 (ng (aref *supremums* index)))
             (declare (uint ok ng))
             (loop until (= (the uint (- ng ok)) 1)
                   for mid = (ldb (byte +bit-width+ 0)
                                  (+ (ash ng -1) (ash ok -1)
                                     (ash (+ (logand ng 1) (logand ok 1)) -1)))
                   when (<= (%power mid index) x)
                   do (setq ok mid)
                   else
                   do (setq ng mid))
             ok))
          ((= index 2) (isqrt x))
          (t x))))
