(defpackage :cp/xorshift
  (:use :cl)
  (:export #:xorshift #:randi #:randprob #:randprob1 #:xorshift-seed! #:randf)
  (:documentation "Provides a random number generator that focuses only on
efficiency."))
(in-package :cp/xorshift)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (assert (= 64 sb-vm:n-word-bits)))

(declaim ((unsigned-byte 64) *state*))
(sb-ext:defglobal *state* (ash 88172645463325252 -1))

;; TODO: investigate the safety of this weird hack
(declaim (inline xorshift))
(defun xorshift (arg)
  "Returns an integer within [0, ARG). Note that this RNG is not strictly
uniform due to modulo bias."
  (declare (optimize (speed 3))
           ((integer 1 #.(ash 1 64)) arg))
  (locally (declare (optimize (safety 0)))
    (let* ((x (sb-kernel:get-lisp-obj-address *state*)))
      (declare ((unsigned-byte 64) x))
      (setq x (ldb (byte 64 0) (logxor x (ash x 7))))
      (setq x (ldb (byte 64 0) (logxor x (ash x -9))))
      (setq *state* (sb-kernel:%make-lisp-obj x))
      (mod x arg))))

(defconstant +skip-count+ 50) ; FIXME: this is not based on any evidence

(defun xorshift-seed! (seed &optional (skip +skip-count+))
  (declare (optimize (speed 3))
           ((integer 1 #.most-positive-fixnum) seed) ; avoid trouble
           ((integer 0 #.most-positive-fixnum) skip))
  (setq *state* seed)
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
