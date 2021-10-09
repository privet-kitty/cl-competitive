(defpackage :cp/bsgs
  (:use :cl)
  (:export #:bsgs))
(in-package :cp/bsgs)

;; NOTE: incomplete & buggy

(deftype uint () '(integer 0 #.most-positive-fixnum))

(defconstant +identity+
  (if (boundp '+identity+)
      (symbol-value '+identity+)
      (make-symbol "IDENTITY")))

(declaim (inline power))
(defun power (base exponent op)
  "OP := binary operation (comprising a monoid)"
  (declare (uint exponent))
  (loop with res = +identity+
        while (> exponent 0)
        when (oddp exponent)
        do (setq res (if (eql res +identity+)
                         base
                         (funcall op res base)))
        do (setq base (funcall op base base)
                 exponent (ash exponent -1))
        finally (return res)))

(declaim (inline bsgs)
         (ftype (function * (values (or null uint) &optional)) bsgs))
(defun bsgs (f applier composer initial-value rhs max-exp &key (test #'eql) from-zero)
  "Finds the least positive (or non-negative, if FROM-ZERO is true) integer k
such that F^k(A) = B.

F is transform, which is usually not a function but a matrix or a scalar. 
APPLIER is function that takes transform f and its argument x, and returns f(x).
COMPOSER composes two transforms.
INITIAL-VALUE is A.
RHS is B.
MAX-EXP is the size of the space."
  (declare ((integer 1 #.most-positive-fixnum) max-exp))
  (let* ((bsize (+ 1 (isqrt (- max-exp 1))))
         (table (make-hash-table :size bsize :test test))
         (fs (make-array (+ 1 bsize) :element-type t))
         (value initial-value)
         (prev-value initial-value)
         (rvalue rhs))
    ;; baby-step
    (dotimes (i (+ bsize 1))
      (when (and (or from-zero (> i 0))
                 (funcall test value rhs))
        (return-from bsgs i))
      (when (< i bsize)
        (setf (gethash rvalue table) i
              value (funcall applier f value)
              rvalue (funcall applier f rvalue))))
    ;; build FS
    (setf (aref fs 1) f)
    (loop for i from 2 to bsize
          do (setf (aref fs i) (funcall composer (aref fs (- i 1)) f)))
    ;; giant-step
    (let* ((f-gs (power f bsize composer)))
      (loop for i from 1 to (ceiling max-exp bsize)
            do (multiple-value-bind (j present-p) (gethash value table)
                 (declare ((or null (integer 0 #.most-positive-fixnum)) j))
                 (when present-p
                   (let* ((lhs (funcall applier (aref fs (- bsize j)) prev-value)))
                     (when (funcall test lhs rhs)
                       (return-from bsgs (- (* i bsize) j))))))
               (setq prev-value value
                     value (funcall applier f-gs value))))))

;; Example usage for discrete logarithm: x^k = y (mod M)
;; (bsgs x
;;       (lambda (x y) (mod (* x y) m))
;;       (lambda (x y) (mod (* x y) m))
;;       (mod 1 m) y m)
