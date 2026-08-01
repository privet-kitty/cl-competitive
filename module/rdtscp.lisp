(defpackage :cp/rdtscp
  (:use :cl :sb-assem)
  (:import-from :sb-c #:defknown #:define-vop #:move)
  (:import-from :sb-vm #:unsigned-reg #:rax-offset #:rdx-offset #:rcx-offset #:unsigned-num)
  (:export #:read-tsc)
  (:documentation "Provides a reader for time-stamp counter using RDTSCP
instruction."))
(in-package :cp/rdtscp)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (sb-ext:unlock-package :sb-x86-64-asm))

(defmacro emit-bytes (segment &rest bytes)
  `(progn ,@(mapcar (lambda (x) `(emit-byte ,segment ,x)) bytes)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun emit-rdtscp (segment)
    (emit-bytes segment #x0f #x01 #xf9))

  (defun register-rdtscp ()
    "Adds RDTSCP to the assembler's instruction set. Later versions of SBCL don't
retain DEFINE-INSTRUCTION in the runtime image, so the encoder has to be
registered by hand."
    (let ((table (symbol-value (find-symbol "*INST-ENCODER*" :sb-assem)))
          (asm-package (symbol-value (find-symbol "*BACKEND-INSTRUCTION-SET-PACKAGE*" :sb-assem))))
      (setf (gethash (intern "RDTSCP" asm-package) table) #'emit-rdtscp)))

  #.(if (find-symbol "DEFINE-INSTRUCTION" :sb-assem)
        '(define-instruction rdtscp (segment)
           (:emitter (emit-rdtscp segment)))
        '(register-rdtscp)))

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
    (dpb hi (byte 32 32) lo)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (sb-ext:lock-package :sb-x86-64-asm))
