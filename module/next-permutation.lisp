(defpackage :cp/next-permutation
  (:use :cl)
  (:export #:no-permutation-error #:next-permutation!)
  (:documentation "Provides linear time computation of the next permutation.

Reference:
https://www.geeksforgeeks.org/find-the-next-lexicographically-greater-word-than-a-given-word/"))
(in-package :cp/next-permutation)

(define-condition no-permutation-error (error)
  ((permutation :initarg :permutation :reader no-permutation-error-permutation))
  (:report
   (lambda (condition stream)
     (format stream "~W is lexicographically maximum."
             (no-permutation-error-permutation condition)))))

(declaim (inline next-permutation!))
(defun next-permutation! (vector &key (order #'<))
  "Destructively changes VECTOR to the lexicographically next permutation
w.r.t. ORDER. ORDER must be a strict order. VECTOR may contain identical
elements."
  (declare (vector vector))
  (let* ((n (length vector))
         (left (- n 2)))
    (declare (fixnum left))
    (loop (when (< left 0)
            (error 'no-permutation-error))
          (when (funcall order (aref vector left) (aref vector (+ left 1)))
            (return))
          (decf left))
    (labels ((bisect (ok ng)
               (declare ((mod #.array-total-size-limit) ok ng))
               (if (<= (- ng ok) 1)
                   ok
                   (let ((mid (ash (+ ok ng) -1)))
                     (if (funcall order (aref vector left) (aref vector mid))
                         (bisect mid ng)
                         (bisect ok mid))))))
      (rotatef (aref vector left) (aref vector (bisect left n)))
      (loop for i from 1 below (ceiling (- n left) 2)
            do (rotatef (aref vector (+ left i))
                        (aref vector (- n i))))
      vector)))
