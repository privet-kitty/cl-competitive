(defpackage :cp/mod128
  (:use :cl)
  (:import-from #:sb-c
                #:deftransform
                #:derive-type
                #:defoptimizer
                #:defknown #:movable #:foldable #:flushable #:commutative
                #:always-translatable
                #:lvar-type
                #:lvar-value
                #:integer-type-numeric-bounds
                #:define-vop)
  (:import-from #:sb-vm
                #:move #:inst #:rax-offset #:rdx-offset
                #:any-reg #:control-stack #:unsigned-reg
                #:positive-fixnum #:unsigned-num #:unsigned-stack)
  (:import-from #:sb-kernel #:specifier-type)
  (:export #:*128)
  (:documentation "Provides modular multiplication of 64-bit integers."))
(in-package :cp/mod128)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun derive-mod (modulus)
    (let ((high (nth-value 1 (integer-type-numeric-bounds (lvar-type modulus)))))
      (specifier-type (if (integerp high)
                          `(integer 0 (,high))
                          `(integer 0))))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defknown *128 ((unsigned-byte 64) (unsigned-byte 64) (unsigned-byte 64))
      (unsigned-byte 64)
      (movable foldable flushable commutative always-translatable)
    :overwrite-fndb-silently t)

  (defoptimizer (*128 derive-type) ((x y mod))
    (declare (ignore x y))
    (derive-mod mod))

  (define-vop (fast-*128/unsigned-num)
    (:translate *128)
    (:policy :fast-safe)
    (:args (x :scs (unsigned-reg) :target rax)
           (y :scs (unsigned-reg unsigned-stack))
           (mod :scs (unsigned-reg unsigned-stack)))
    (:arg-types unsigned-num unsigned-num unsigned-num)
    (:temporary (:sc unsigned-reg :offset rax-offset
                 :from (:argument 0) :to :result) rax) ; lo; quot
    (:temporary (:sc unsigned-reg :offset rdx-offset :target res
                 :from :load :to :result) rdx) ; hi; rem
    (:results (res :scs (unsigned-reg)))
    (:result-types unsigned-num)
    (:note "inline *128")
    (:vop-var vop)
    (:save-p :compute-only)
    (:generator
     10
     (move rax x)
     (inst mul rax y)
     (inst idiv rax mod)
     (move res rdx))))
