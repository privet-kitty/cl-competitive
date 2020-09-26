;;;
;;; mod-inverse with safeguard against division-by-zero
;;;

(defpackage :cp/experimental/mod-inverse
  (:use :cl)
  (:import-from :sb-c #:lvar-type #:integer-type-numeric-bounds)
  (:export #:mod-inverse))
(in-package :cp/experimental/mod-inverse)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (sb-c:defknown %mod-inverse ((integer 0) (integer 1)) (integer 0)
      (sb-c:flushable sb-c:foldable)
    :overwrite-fndb-silently t)
  (sb-c:defknown mod-inverse (integer (integer 1)) (integer 0)
      (sb-c:flushable sb-c:foldable)
    :overwrite-fndb-silently t)
  (defun derive-mod (modulus)
    (let ((high (nth-value 1 (integer-type-numeric-bounds (lvar-type modulus)))))
      (sb-kernel:specifier-type (if (integerp high)
                                    `(integer 0 (,high))
                                    `(integer 0)))))
  (sb-c:defoptimizer (%mod-inverse sb-c:derive-type) ((integer modulus))
    (declare (ignore integer))
    (derive-mod modulus))
  (sb-c:defoptimizer (mod-inverse sb-c:derive-type) ((integer modulus))
    (declare (ignore integer))
    (derive-mod modulus)))

(defun %mod-inverse (integer modulus)
  (declare (optimize (speed 3) (safety 0))
           (sb-ext:muffle-conditions sb-ext:compiler-note))
  (macrolet ((frob (utype stype)
               `(let ((a integer)
                      (b modulus)
                      (u 1)
                      (v 0))
                  (declare (,stype a b u v))
                  (loop until (zerop b)
                        for quot = (floor a b)
                        do (decf a (the ,stype (* quot b)))
                           (rotatef a b)
                           (decf u (the ,stype (* quot v)))
                           (rotatef u v))
                  (the ,utype (if (< u 0)
                                  (+ u modulus)
                                  u)))))
    (typecase modulus
      ((unsigned-byte 31) (frob (unsigned-byte 31) (signed-byte 32)))
      ((unsigned-byte 62) (frob (unsigned-byte 62) (signed-byte 63)))
      (otherwise (frob (integer 0) integer)))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (locally (declare (sb-ext:muffle-conditions style-warning))
    (sb-c:deftransform mod-inverse ((integer modulus) * * :node node)
      "inline modular inverse"
      (let ((result (gensym)))
        `(let* ((integer (mod integer modulus))
                (,result (%mod-inverse integer modulus)))
           ,@(when (sb-c:policy node (>= safety 1))
               `((unless (or (= 1 (rem (* integer ,result) modulus)) (= 1 modulus))
                   (error 'division-by-zero
                          :operands (list integer modulus)
                          :operation 'mod-inverse))))
           ,result)))))

(defun mod-inverse (integer modulus)
  "Solves ax ≡ 1 mod m. INTEGER and MODULUS must be coprime. Signals
DIVISION-BY-ZERO for a non-coprime input when safety >= 1."
  (declare (optimize (speed 3))
           (sb-ext:muffle-conditions sb-ext:compiler-note))
  (mod-inverse integer modulus))
