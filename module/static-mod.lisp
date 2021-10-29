(defpackage :cp/static-mod
  (:use :cl)
  (:export #:+mod+))
(in-package :cp/static-mod)

(defconstant +mod+ (if (boundp 'cl-user::+mod+)
                       (symbol-value 'cl-user::+mod+)
                       998244353))
