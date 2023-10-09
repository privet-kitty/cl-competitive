(defpackage :cp/hermite-normal-form
  (:use :cl)
  (:import-from :cp/ext-gcd #:ext-gcd)
  (:export #:hnf! #:hnf-p)
  (:documentation "Provides an algorithm to compute the Hermite normal form."))
(in-package :cp/hermite-normal-form)

;; TODO: It would be better to have U as a sequence of operations than to have
;; it as an n * n matrix, especially when n is large.
(declaim (inline hnf!))
(defun hnf! (matrix)
  "Destructively modifies the given matrix A to the Hermite normal form H and
returns H. Additionally returns the unimodular matrix U such that AU = H as the
second value.

NOTE: A must have full row rank. Otherwise the behaviour is undefined.

Although U is not unique, this function is deterministic: i.e. it returns the
same U for the same A.

This algorithm requires O(mn^2) arithmetic operations as well as O(mn) runs of
extended Euclidean algorithm. If you don't need the unimodular matrix, this can
be reduced to O(m^2n). It doesn't mean that this is a polynomial algorithm,
however. For details, please see the reference.

Reference:
Jünger et al. 50 Years of Integer Programming 1958-2008. p.511."
  (declare ((array * (* *)) matrix))
  (destructuring-bind  (m n) (array-dimensions matrix)
    (declare ((integer 0 (#.array-dimension-limit)) m n))
    (assert (<= m n))
    (let ((u (make-array (list n n) :element-type (array-element-type matrix) :initial-element 0)))
      (dotimes (i n)
        (setf (aref u i i) 1))
      (dotimes (i m)
        (loop with hi = (aref matrix i i)
              for j from (+ i 1) below n
              for hj = (aref matrix i j)
              unless (zerop (aref matrix i j))
              do (multiple-value-bind (x y) (ext-gcd (aref matrix i i) (aref matrix i j))
                   (let* ((g (+ (* x (aref matrix i i)) (* y (aref matrix i j))))
                          (hi/g (floor (aref matrix i i) g))
                          (-hj/g (- (floor (aref matrix i j) g))))
                     (dotimes (k m)
                       (let ((new-i (+ (* (aref matrix k i) x)
                                       (* (aref matrix k j) y)))
                             (new-j (+ (* (aref matrix k i) -hj/g)
                                       (* (aref matrix k j) hi/g))))
                         (setf (aref matrix k i) new-i
                               (aref matrix k j) new-j)))
                     (dotimes (k n)
                       (let ((new-i (+ (* (aref u k i) x)
                                       (* (aref u k j) y)))
                             (new-j (+ (* (aref u k i) -hj/g)
                                       (* (aref u k j) hi/g))))
                         (setf (aref u k i) new-i
                               (aref u k j) new-j))))))
        (when (< (aref matrix i i) 0)
          (dotimes (k m)
            (setf (aref matrix k i) (- (aref matrix k i))))
          (dotimes (k n)
            (setf (aref u k i) (- (aref u k i)))))
        (dotimes (j i)
          (let ((q (if (>= (aref matrix i i) 0)
                       (floor (aref matrix i j) (aref matrix i i))
                       (ceiling (aref matrix i j) (aref matrix i i)))))
            (dotimes (k m)
              (decf (aref matrix k j) (* q (aref matrix k i))))
            (dotimes (k n)
              (decf (aref u k j) (* q (aref u k i)))))))
      (values matrix u))))

(declaim (inline hnf-p))
(defun hnf-p (matrix)
  "Tests if MATRIX is in the Hermite normal form.

Note that currently this function assumes that MATRIX has full row rank."
  (declare ((array * (* *)) matrix))
  (destructuring-bind  (m n) (array-dimensions matrix)
    (declare ((integer 0 (#.array-dimension-limit)) m n))
    (assert (<= m n))
    (loop for i below m
          for diag-elm = (aref matrix i i)
          always (and (> diag-elm 0)
                      (loop for j below i
                            always (< -1 (aref matrix i j) diag-elm))
                      (loop for j from (+ i 1) below n
                            always (zerop (aref matrix i j)))))))
