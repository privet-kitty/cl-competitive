(defpackage :cp/bsgs
  (:use :cl)
  (:export #:bsgs-action))
(in-package :cp/bsgs)

;; NOTE: incomplete

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

(declaim (inline bsgs-action)
         (ftype (function * (values (or null uint) &optional)) bsgs-action))
(defun bsgs-action (f action op initial-value max &key (test #'eql))
  "Finds the least positive integer k such that F^k * A = A, where * is a left
action. Time complexity is O(sqrt(MAX)).

NOTE: This function also accepts input of type F^k * A = B, but in this case the
result is not necessarily correct. (E.g. discrete logarithm problem; please use
cp/mod-log instead.)

Let S be a semigroup. F is an element of S. ACTION is a two-variable function
that expresses *. OP is a monoid operation on S, i.e., S \times S -> S.
INITIAL-VALUE is A. MAX is the upper bound of k."
  (declare ((integer 1 #.most-positive-fixnum) max))
  (let* ((bsize (+ 1 (isqrt (- max 1))))
         (table (make-hash-table :size bsize :test test))
         (fs (make-array (+ 1 bsize) :element-type t))
         (value initial-value)
         (prev-value initial-value))
    ;; baby step
    (dotimes (i bsize)
      (when (and (> i 0) (funcall test value initial-value))
        (return-from bsgs-action i))
      ;; NOTE: This function can terminate if a cycle is found, but this clause
      ;; could make it rather slower, because access to hash-table can be a
      ;; bottleneck.
      ;; (when (gethash rvalue table)
      ;;   (return-from bsgs-action))
      (setf (gethash value table) i
            value (funcall action f value)))
    ;; build FS
    (setf (aref fs 1) f)
    (loop for i from 2 to bsize
          do (setf (aref fs i) (funcall op (aref fs (- i 1)) f)))
    ;; giant step
    (let* ((f-gs (power f bsize op)))
      (loop for i from 1 to (ceiling max bsize)
            do ;; heuristic for A != B input
               (when (funcall test value initial-value)
                 (return-from bsgs-action (* i bsize)))
               (multiple-value-bind (j present-p) (gethash value table)
                 (declare ((or null (integer 0 #.most-positive-fixnum)) j))
                 (when present-p
                   (let* ((lhs (funcall action (aref fs (- bsize j)) prev-value)))
                     (when (funcall test lhs initial-value)
                       (return-from bsgs-action (- (* i bsize) j))))))
               (setq prev-value value
                     value (funcall action f-gs value))))))

(declaim (ftype (function * (values (or null uint) &optional)) %bsgs-cycle-length))
(defun %bsgs-cycle-length (f action op initial-value max &key (test #'eql))
  "Finds the cycle length of the sequence x, F*x, (F^2)*x, ... in expected
O(sqrt(MAX)) time.

Let S be a semigroup. F is an element of S. ACTION is a two-variable function
that expresses *. OP is a monoid operation on S, i.e., S \times S -> S.
INITIAL-VALUE is x. MAX is the upper bound of the element order."
  (declare ((integer 1 #.most-positive-fixnum) max))
  (let* ((bsize (+ 1 (isqrt (- max 1))))
         (table (make-hash-table :size bsize :test test))
         (f^n (power f max op))
         (value (funcall action f^n initial-value)))
    ;; baby step
    (dotimes (i bsize)
      (let ((prev (gethash value table)))
        (declare ((or null uint) prev))
        (when prev
          (return-from %bsgs-cycle-length (- i prev))))
      (setf (gethash value table) i
            value (funcall action f value)))
    ;; giant step
    (loop with f-gs = (power f bsize op)
          for i from 1 to (ceiling max bsize)
          for j of-type (or null uint) = (gethash value table)
          when j
          do (return-from %bsgs-cycle-length (- (* i bsize) j))
          do (setq value (funcall action f-gs value)))))
