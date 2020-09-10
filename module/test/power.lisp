(defpackage :cp/test/power
  (:use :cl :fiveam :cp/power :cp/mod-operations :cp/gemm)
  (:import-from :cp/test/base #:base-suite))
(in-package :cp/test/power)
(in-suite base-suite)

(defconstant +mod+ (+ (expt 10 9) 7))
(define-mod-operations +mod+)

(defun power1 (mat exp)
  (let ((iden (make-array '(3 3)
                          :element-type '(unsigned-byte 31)
                          :initial-contents '((1 0 0) (0 1 0) (0 0 1)))))
    (power mat exp
           (lambda (mat1 mat2)
             (gemm mat1 mat2 :op+ #'mod+ :op* #'mod*))
           iden)))

(defun power2 (mat exp)
  (let ((res (make-array '(3 3)
                         :element-type '(unsigned-byte 31)
                         :initial-contents '((1 0 0) (0 1 0) (0 0 1)))))
    (dotimes (_ exp)
      (setq res (gemm res mat :op+ #'mod+ :op* #'mod*)))
    res))

(test power/random
  (let ((state (sb-ext:seed-random-state 0)))
    (finishes
      (dotimes (_ 500)
        (let ((mat (make-array '(3 3) :element-type '(unsigned-byte 31) :initial-element 0)))
          (dotimes (i 3)
            (dotimes (j 3)
              (setf (aref mat i j) (random +mod+ state))))
          (let ((exp (random 50 state)))
            (assert (equalp (power1 mat exp) (power2 mat exp)))))))))
