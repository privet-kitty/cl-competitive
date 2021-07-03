(defpackage :cp/xorshift
  (:use :cl)
  (:export #:xorshift #:randi #:randprob #:randprob1 #:xorshift-seed! #:randf)
  (:documentation "Provides a random number generator that focuses only on
efficiency."))
(in-package :cp/xorshift)

(declaim ((simple-array (unsigned-byte 64) (1)) *state*))
(sb-ext:defglobal *state*
  (make-array 1 :element-type '(unsigned-byte 64) :initial-element 88172645463325252))

(declaim (inline xorshift))
(defun xorshift (arg)
  "Returns an integer within [0, ARG). Note that this RNG is not strictly
uniform because of modulo bias."
  (declare (optimize (speed 3))
           ((integer 1 #.(ash 1 64)) arg))
  (locally (declare (optimize (safety 0)))
    (let* ((state *state*)
           (x (aref state 0)))
      (declare ((unsigned-byte 64) x))
      (setq x (ldb (byte 64 0) (logxor x (ash x 7))))
      (setq x (ldb (byte 64 0) (logxor x (ash x -9))))
      (mod (setf (aref state 0) x) arg))))

(defconstant +skip-count+ 50) ; FIXME: this is not based on any evidence

(defun xorshift-seed! (seed &optional (skip +skip-count+))
  (declare (optimize (speed 3))
           ((integer 1 (#.(ash 1 64))) seed)
           ((integer 0 #.most-positive-fixnum) skip))
  (setf (aref *state* 0) seed)
  (dotimes (_ skip)
    (xorshift 1)))

(declaim (inline randi))
(defun randi (l r)
  "Returns an integer within [L, R)."
  (declare (optimize (speed 3))
           (fixnum l r))
  (+ l (xorshift (- r l))))

(defconstant +epsilon+ (float (/ (ash 1 53)) 1d0))

(declaim (inline randprob)
         (ftype (function * (values (double-float 0d0 (1d0)) &optional)) randprob))
(defun randprob ()
  "Returns a double-float within [0, 1)."
  (* +epsilon+ (xorshift #.(ash 1 53))))

(declaim (inline randprob1))
(defun randprob1 ()
  "Returns a double-float within [0, 1]."
  (* double-float-epsilon (xorshift #.(ash 1 53))))

(declaim (inline randf))
(defun randf (l r)
  (+ l (* (- r l) (randprob))))

;; (defun test (sample)
;;   (declare (optimize (speed 3) (safety 0))
;;            ((integer 0 #.most-positive-fixnum) sample))
;;   (let ((res 0))
;;     (declare ((unsigned-byte 62) res))
;;     (dotimes (_ sample)
;;       (incf res (xorshift 2)))
;;     res))
