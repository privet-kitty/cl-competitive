(defpackage :cp/clamp
  (:use :cl)
  (:export #:clamp))
(in-package :cp/clamp)

;; From alexandria
(declaim (inline clamp))
(defun clamp (number min max)
  (if (< number min)
      min
      (if (> number max)
          max
          number)))
