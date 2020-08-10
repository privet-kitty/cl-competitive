(defpackage :cp/modify-macro
  (:use :cl)
  (:export #:minf #:maxf #:mulf #:divf #:iorf #:xorf #:andf))
(in-package :cp/modify-macro)

(macrolet ((def (name fname)
             `(define-modify-macro ,name (new-value) ,fname)))
  (def minf min)
  (def maxf max)
  (def mulf *)
  (def divf /)
  (def iorf logior)
  (def xorf logxor)
  (def andf logand))
