(defpackage :cp/xorshift
  (:use :cl)
  (:export #:xorshift #:randi #:randprob #:xorshift-seed!))
(in-package :cp/xorshift)

(declaim ((simple-array (unsigned-byte 64) (1)) *state*))
(sb-ext:defglobal *state*
  (make-array 1 :element-type '(unsigned-byte 64) :initial-element 88172645463325252))

(defconstant +skip-count+ 50) ; FIXME: this is not based on any evidence

(declaim (inline xorshift))
(defun xorshift (arg)
  "Note that this RNG is not strictly uniform."
  (declare (optimize (speed 3))
           ((integer 1 #.(ash 1 64)) arg))
  (locally (declare (optimize (safety 0)))
    (let* ((state *state*)
           (x (aref state 0)))
      (declare ((unsigned-byte 64) x))
      (setq x (ldb (byte 64 0) (logxor x (ash x 7))))
      (setq x (ldb (byte 64 0) (logxor x (ash x -9))))
      (mod (setf (aref state 0) x) arg))))

(defun xorshift-seed! (seed &optional (skip +skip-count+))
  (declare (optimize (speed 3))
           ((integer 1 (#.(ash 1 64))) seed)
           ((integer 0 #.most-positive-fixnum) skip))
  (setf (aref *state* 0) seed)
  (dotimes (_ skip)
    (xorshift 1)))

(declaim (inline randi))
(defun randi (l r)
  (declare (optimize (speed 3))
           ((integer 0 #.most-positive-fixnum) l r))
  (sb-ext:truly-the (integer 0 #.most-positive-fixnum)
                    (+ l (xorshift (- r l)))))

(defconstant +unit+ (float (/ (ash 1 53)) 1d0))

(declaim (inline randprob)
         (ftype (function * (values (double-float 0d0 #.(* +unit+ (- (ash 1 53) 1)))
                                    &optional))
                randprob))
(defun randprob ()
  (* +unit+ (xorshift #.(ash 1 53))))

;; (defun test (sample)
;;   (declare (optimize (speed 3) (safety 0))
;;            ((integer 0 #.most-positive-fixnum) sample))
;;   (let ((res 0))
;;     (declare ((unsigned-byte 62) res))
;;     (dotimes (_ sample)
;;       (incf res (xorshift 2)))
;;     res))
