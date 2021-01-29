(defpackage :cp/experimental/barrett
  (:use :cl)
  (:import-from :sb-ext #:truly-the)
  (:import-from :sb-c
                #:deftransform
                #:derive-type
                #:defoptimizer
                #:defknown #:movable #:foldable #:flushable #:commutative
                #:lvar-type
                #:lvar-value
                #:give-up-ir1-transform
                #:integer-type-numeric-bounds
                #:define-vop)
  (:import-from :sb-vm
                #:move #:inst #:eax-offset #:edx-offset
                #:any-reg #:control-stack #:unsigned-reg
                #:positive-fixnum
                #:fixnumize #:ea)
  (:import-from :sb-kernel #:specifier-type)
  (:import-from :sb-int #:explicit-check #:constant-arg)
  (:export #:fast-mod #:%himod)
  (:documentation "Provides Barrett reduction."))
(in-package :cp/experimental/barrett)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun derive-* (x y)
    (let ((high1 (nth-value 1 (integer-type-numeric-bounds (lvar-type x))))
          (high2 (nth-value 1 (integer-type-numeric-bounds (lvar-type y)))))
      (specifier-type (if (and (integerp high1) (integerp high2))
                          `(integer 0 ,(ash (* high1 high2) -62))
                          `(integer 0)))))

  (defun derive-mod (modulus)
    (let ((high (nth-value 1 (integer-type-numeric-bounds (lvar-type modulus)))))
      (specifier-type (if (integerp high)
                          `(integer 0 (,high))
                          `(integer 0)))))

  ;; *-high62
  (defknown *-high62 ((unsigned-byte 62) (unsigned-byte 62)) (unsigned-byte 62)
      (movable foldable flushable commutative sb-c::always-translatable)
    :overwrite-fndb-silently t)

  (defoptimizer (*-high62 derive-type) ((x y))
    (derive-* x y))

  (define-vop (fast-*-high62/fixnum)
    (:translate *-high62)
    (:policy :fast-safe)
    (:args (x :scs (any-reg) :target eax)
           (y :scs (any-reg control-stack)))
    (:arg-types positive-fixnum positive-fixnum)
    (:temporary (:sc any-reg :offset eax-offset
                 :from (:argument 0) :to :result)
                eax)
    (:temporary (:sc any-reg :offset edx-offset :target r
                 :from :eval :to :result)
                edx)
    (:results (r :scs (any-reg)))
    (:result-types positive-fixnum)
    (:note "inline *-high62")
    (:vop-var vop)
    (:save-p :compute-only)
    (:generator 6
                (move eax x)
                (inst mul eax y)
                (inst shl edx 1)
                (move r edx)))

  (define-vop (fast-c-*-high62-/fixnum)
    (:translate *-high62)
    (:policy :fast-safe)
    (:args (x :scs (any-reg) :target eax))
    (:info y)
    (:arg-types positive-fixnum (:constant (unsigned-byte 62)))
    (:temporary (:sc any-reg :offset eax-offset
                 :from (:argument 0) :to :result)
                eax)
    (:temporary (:sc any-reg :offset edx-offset :target r
                 :from :eval :to :result)
                edx)
    (:results (r :scs (any-reg)))
    (:result-types positive-fixnum)
    (:note "inline constant *-high62")
    (:vop-var vop)
    (:save-p :compute-only)
    (:generator 6
                (move eax x)
                (inst mul eax (sb-c:register-inline-constant :qword y))
                (inst shl edx 1)
                (move r edx)))

  (defun *-high62 (x y)
    (declare (explicit-check))
    (*-high62 x y))

  ;; %himod
  (defknown %himod ((unsigned-byte 32) (unsigned-byte 31)) (unsigned-byte 31)
      (movable foldable flushable sb-c:always-translatable)
    :overwrite-fndb-silently t)

  (defoptimizer (%himod derive-type) ((integer modulus))
    (declare (ignore integer))
    (derive-mod modulus))

  (define-vop (fast-c-%himod)
    (:translate %himod)
    (:policy :fast-safe)
    (:args (x :scs (any-reg) :target r))
    (:info m)
    (:arg-types positive-fixnum (:constant (unsigned-byte 31)))
    (:temporary (:sc any-reg :from :eval :to :result) y)
    (:results (r :scs (any-reg)))
    (:result-types positive-fixnum)
    (:note "inline constant %himod")
    (:vop-var vop)
    (:generator 4
                (assert #.(if (find-symbol "GPR-TN-P" :sb-vm)
                              `(funcall (intern "GPR-TN-P" :sb-vm) x)
                              t))
                (when (sb-c:tn-p m)
                  (assert (sb-c:sc-is m sb-vm::immediate))
                  (setq m (sb-c::tn-value m)))
                (setq m (fixnumize m))
                (move r x)
                (inst cmp r (- m 2))
                (inst lea y #.(if (fboundp 'ea)
                                  `(sb-vm::ea (- m) r)
                                  `(sb-vm::make-ea :dword :disp (- m) :base r)))
                (inst cmov :a r y)))

  ;; fast-mod
  (defknown fast-mod (integer unsigned-byte) unsigned-byte
      (movable foldable flushable)
    :overwrite-fndb-silently t)

  (defoptimizer (fast-mod derive-type) ((integer modulus))
    (declare (ignore integer))
    (derive-mod modulus))

  (defun fast-mod (number divisor)
    "Is equivalent to CL:MOD for integer arguments"
    (declare (explicit-check))
    (mod number divisor))

  (deftransform fast-mod ((number divisor)
                          ((unsigned-byte 62) (constant-arg (unsigned-byte 31))) *
                          :important t)
    "convert FAST-MOD to barrett reduction"
    (let ((mod (lvar-value divisor)))
      (if (<= mod 1)
          (give-up-ir1-transform)
          (let ((m (floor (ash 1 62) mod)))
            `(let* ((q (*-high62 number ,m))
                    (x (truly-the (signed-byte 32) (- number (* q ,mod)))))
               (if (< x ,mod) x (- x ,mod))
               ;; Not so effective because branch prediction is usually correct?
               ;; (sb-ext:truly-the (mod ,mod) (%himod x ,mod))
               ))))))

;; (defun bench ()
;;   (declare (optimize (speed 3) (safety 0)))
;;   (loop for x from #.(expt 10 9) to #.(expt 10 18) by #.(expt 10 9)
;;         sum (logand 1 (mod x 998244353))
;;         of-type (unsigned-byte 62)))

;; (defun bench-fast ()
;;   (declare (optimize (speed 3) (safety 0)))
;;   (loop for x from #.(expt 10 9) to #.(expt 10 18) by #.(expt 10 9)
;;         sum (logand 1 (fast-mod x 998244353))
;;         of-type (unsigned-byte 62)))

;; #-swank
;; (progn (time (bench))
;;        (time (bench-fast)))
