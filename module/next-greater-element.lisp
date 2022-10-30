(defpackage :cp/next-greater-element
  (:use :cl)
  (:export #:make-nge-vector))
(in-package :cp/next-greater-element)

;; not tested
(declaim (inline make-nge-vector))
(defun make-nge-vector (vector order &key from-end (none -1))
  "Returns a vector of indices of next greater (or less, depending on ORDER)
elements. The empty place is filled with NONE. Below are examples of ORDER:

<: next greater
<=: next greater or equal
>: next less
>=: next less or equal

If FROM-END is true, this function deals with previous greater or less elements
instead. The arguments passed to ORDER are in ascending order of position
regardless of FROM-END. (In other words, it is always consistent with the visual
order.)"
  (declare (vector vector)
           (fixnum none))
  (let* ((n (length vector))
         (res (make-array n :element-type 'fixnum :initial-element none))
         stack)
    (if from-end
        (loop for i from (- n 1) downto 0
              for x = (aref vector i)
              do (loop (unless stack
                         (return))
                       (let ((prev (car stack)))
                         (unless (funcall order x (aref vector prev))
                           (return))
                         (setf (aref res prev) i)
                         (pop stack)))
                 (push i stack))
        (loop for i below n
              for x = (aref vector i)
              do (loop (unless stack
                         (return))
                       (let ((prev (car stack)))
                         (unless (funcall order (aref vector prev) x)
                           (return))
                         (setf (aref res prev) i)
                         (pop stack)))
                 (push i stack)))
    res))
