(defpackage :cp/extend-vector
  (:use :cl)
  (:export #:extend-vector #:extend-vectorf #:vector-set*))
(in-package :cp/extend-vector)

(declaim (inline %power-of-two-ceiling))
(defun %power-of-two-ceiling (x)
  (ash 1 (integer-length (- x 1))))

(defun extend-vector (vector size &optional (initial-element nil supplied-p))
  (declare (optimize (speed 3))
           ((mod #.array-dimension-limit) size)
           (vector vector))
  (let ((new-size (%power-of-two-ceiling (max size 1))))
    (declare ((mod #.array-dimension-limit) new-size))
    (if (< (length vector) new-size)
        (if supplied-p
            (adjust-array vector new-size :initial-element initial-element)
            (adjust-array vector new-size))
        vector)))

(defconstant +not-supplied+ (if (boundp '+not-supplied+)
                                (symbol-value '+not-supplied+)
                                'not-supplied))

(define-modify-macro extend-vectorf (new-size &optional (initial-element '+not-supplied+))
  (lambda (vector new-size initial-element)
    (declare ((mod #.array-dimension-limit) new-size))
    (if (< (length vector) new-size)
        (if (eq initial-element +not-supplied+)
            (extend-vector vector new-size)
            (extend-vector vector new-size initial-element))
        vector)))

(defmacro vector-set* (vector index new-element)
  (let ((i (gensym))
        (elm (gensym)))
    ;; TODO: use setf-expander
    `(let ((,i ,index)
           (,elm ,new-element))
       (extend-vectorf ,vector (+ 1 ,i))
       (setf (aref ,vector ,i) ,elm))))
