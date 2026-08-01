(defpackage :cp/bmi
  (:use :cl #:sb-assem)
  (:import-from #:sb-x86-64-asm #:emit-vex #:emit-ea)
  (:import-from #:sb-c #:define-vop)
  (:import-from #:sb-vm #:unsigned-reg #:unsigned-stack #:unsigned-num)
  (:import-from #:sb-sys #:%primitive)
  (:export #:pdep))
(in-package :cp/bmi)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (sb-ext:unlock-package :sb-x86-64-asm))

(defmacro emit-bytes (segment &rest bytes)
  `(progn ,@(mapcar (lambda (x) `(emit-byte ,segment ,x)) bytes)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun emit-vex-inst (segment thing reg prefix opcode-prefix opcode vvvv l w)
    (emit-vex segment vvvv thing reg prefix opcode-prefix l w)
    (emit-bytes segment opcode)
    (emit-ea segment thing reg))

  ;; https://www.felixcloutier.com/x86/pdep
  (defun emit-pdep (segment dst src src2)
    ;; FIXME: should we always use W=1?
    (emit-vex-inst segment src2 dst #xf2 #x0f38 #xf5 src 0 1))

  (defun register-pdep ()
    "Adds PDEP to the assembler's instruction set. Later versions of SBCL don't
retain DEFINE-INSTRUCTION in the runtime image, so the encoder has to be
registered by hand."
    (let ((table (symbol-value (find-symbol "*INST-ENCODER*" :sb-assem)))
          (asm-package (symbol-value (find-symbol "*BACKEND-INSTRUCTION-SET-PACKAGE*" :sb-assem))))
      (setf (gethash (intern "PDEP" asm-package) table) #'emit-pdep))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  #.(if (find-symbol "DEFINE-INSTRUCTION" :sb-assem)
        '(define-instruction pdep (segment dst src src2)
           (:emitter (emit-pdep segment dst src src2)))
        '(register-pdep))

  (define-vop (pdep)
    (:policy :fast-safe)
    (:args (src :scs (unsigned-reg))
           (mask :scs (unsigned-reg unsigned-stack)))
    (:arg-types unsigned-num unsigned-num)
    (:results (res :scs (unsigned-reg)))
    (:result-types unsigned-num)
    (:generator
     8
     (inst pdep res src mask))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (sb-ext:lock-package :sb-x86-64-asm))
