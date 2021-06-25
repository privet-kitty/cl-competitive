(defpackage :cp/rdtscp
  (:use :cl :sb-assem)
  (:import-from :sb-c #:defknown #:define-vop #:move)
  (:import-from :sb-vm #:unsigned-reg #:rax-offset #:rdx-offset #:rcx-offset #:unsigned-num)
  (:export #:read-tsc)
  (:documentation "Provides a reader for time-stamp counter using RDTSCP
instruction."))
(in-package :cp/rdtscp)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (when (find-package :sb-x86-64-asm)
    (sb-ext:unlock-package :sb-x86-64-asm)))

(defmacro emit-bytes (segment &rest bytes)
  `(progn ,@(mapcar (lambda (x) `(emit-byte ,segment ,x)) bytes)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (define-instruction rdtscp (segment)
    (:emitter (emit-bytes segment #x0f #x01 #xf9))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defknown %read-tsc () (values (unsigned-byte 32) (unsigned-byte 32)) ()
    :overwrite-fndb-silently t)
  (define-vop (%read-tsc)
    (:policy :fast-safe)
    (:translate %read-tsc)
    (:temporary (:sc unsigned-reg :offset rax-offset :target lo) eax)
    (:temporary (:sc unsigned-reg :offset rdx-offset :target hi) edx)
    ;; RDTSCP instruction reads IA32_TSC_AUX value into ECX
    (:temporary (:sc unsigned-reg :offset rcx-offset) ecx)
    (:ignore ecx)
    (:results (hi :scs (unsigned-reg))
              (lo :scs (unsigned-reg)))
    (:result-types unsigned-num unsigned-num)
    (:generator 3
                (inst rdtscp)
                (move lo eax)
                (move hi edx))))

(declaim (inline read-tsc))
(defun read-tsc ()
  (multiple-value-bind (hi lo) (%read-tsc)
    (dpb hi (byte 30 32) lo)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (when (find-package :sb-x86-64-asm)
    (sb-ext:lock-package :sb-x86-64-asm)))
