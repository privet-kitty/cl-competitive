(defpackage :cp/integer-hash
  (:use :cl)
  (:export #:%get-time-since-epoch #:integer-hash))
(in-package :cp/integer-hash)

;; Reference:
;; https://codeforces.com/blog/entry/62393
;; http://xoshiro.di.unimi.it/splitmix64.c

;; May be used for seeding random state at compile-time
(declaim (ftype (function * (values (unsigned-byte 62) &optional))
                %get-time-since-epoch))
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun %get-time-since-epoch ()
    "Returns timeeconds since Unix epoch."
    #+sbcl (multiple-value-bind (sec microsec) (sb-ext:get-time-of-day)
             (+ (* sec #.(expt 10 6)) microsec))
    ;; See https://lisptips.com/post/11649360174/the-common-lisp-and-unix-epochs
    #-sbcl (error "Not implemented")))

(declaim ((unsigned-byte 62) *time-since-epoch*))
(sb-ext:define-load-time-global *time-since-epoch* (%get-time-since-epoch))

(declaim (ftype (function * (values (unsigned-byte 62) &optional)) %splitmix64))
(defun %splitmix64 (x)
  (declare (optimize (speed 3))
           ((unsigned-byte 64) x))
  (setq x (ldb (byte 64 0) (+ x #x9e3779b97f4a7c15)))
  (setq x (ldb (byte 64 0) (* (logxor x (ash x -30))
                              #xbf58476d1ce4e5b9)))
  (setq x (ldb (byte 64 0) (* (logxor x (ash x -27))
                              #x94d049bb133111eb)))
  (ldb (byte 62 0) (logxor x (ash x -31))))

;; NOTE: This function accepts bignum but returns a periodic value modulo 2^62.
(declaim (inline integer-hash))
(defun integer-hash (x)
  (declare (integer x))
  (%splitmix64 (ldb (byte 62 0) (+ x *time-since-epoch*))))
