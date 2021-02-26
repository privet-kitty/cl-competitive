(defpackage :cp/log-prefix-p
  (:use :cl :cp/tzcount)
  (:export #:log-prefix-p))
(in-package :cp/log-prefix-p)

;; FIXME: any smarter ways?
(declaim (inline log-prefix-p))
(defun log-prefix-p (prefix integer)
  "Returns true iff PREFIX is a prefix of INTEGER in a binary
representation. Note that 0 is a prefix of any integers."
  (or (= prefix integer)
      (let ((xor (logxor prefix integer)))
        (>= (tzcount (logand xor (- xor)))
            (integer-length prefix)))))
