(defpackage :cp/static-mod
  (:use :cl)
  (:export #:+mod+ #:mint #:mint-vector))
(in-package :cp/static-mod)

(defconstant +mod+ (if (boundp 'cl-user::+mod+)
                       (symbol-value 'cl-user::+mod+)
                       998244353))
(deftype mint () '(unsigned-byte 31))
(deftype mint-vector () '(simple-array mint (*)))
