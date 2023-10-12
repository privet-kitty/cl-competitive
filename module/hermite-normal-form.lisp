(defpackage :cp/hermite-normal-form
  (:use :cl)
  (:import-from :cp/ext-gcd #:ext-gcd)
  (:import-from :cp/bareiss #:bareiss!)
  (:export #:hnf! #:%hnf-fast! #:hnf-p)
  (:documentation "Provides an algorithm to compute the Hermite normal form.

Reference:
Jünger et al. 50 Years of Integer Programming 1958-2008. p.511.
"))
(in-package :cp/hermite-normal-form)

;; TODO: It would be better to have U as a sequence of operations than to have
;; it as an n * n matrix, especially when n is large.

(declaim (inline hnf!))
(defun hnf! (matrix)
  "Returns the hermite normal form H of the given m * n matrix A such that m <= n,
and returns the unimodular matrix U such that AU = H as the second value. This
function destructively modifies MATRIX.

Actually the returned H is the same object as MATRIX, but I don't recommend that
you exploit this behaviour.

NOTE: A must have full row rank. Otherwise the behaviour is undefined.

Although U is not unique, this function is deterministic: i.e. it returns the
same U for the same A.

This algorithm requires O(mn^2) arithmetic operations as well as O(mn) extended
Euclidean algorithm operations. If you don't need the unimodular matrix, this
can be reduced to O(m^2n). However, that does NOT necessarily mean that this is
a polynomial algorithm because the size of the numbers that appear in the
computation may grow exponentially. For details, please see the reference."
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
                     ;; A_{k, j} = 0 for all k < i
                     (loop for k from i below m
                           for value-i = (+ (* (aref matrix k i) x)
                                            (* (aref matrix k j) y))
                           for value-j = (+ (* (aref matrix k i) -hj/g)
                                            (* (aref matrix k j) hi/g))
                           do (setf (aref matrix k i) value-i
                                    (aref matrix k j) value-j))
                     (loop for k below n
                           for value-i = (+ (* (aref u k i) x)
                                            (* (aref u k j) y))
                           for value-j = (+ (* (aref u k i) -hj/g)
                                            (* (aref u k j) hi/g))
                           do (setf (aref u k i) value-i
                                    (aref u k j) value-j)))))
        (when (< (aref matrix i i) 0)
          (dotimes (k m)
            (setf (aref matrix k i) (- (aref matrix k i))))
          (dotimes (k n)
            (setf (aref u k i) (- (aref u k i)))))
        (dotimes (j i)
          (let ((q (floor (aref matrix i j) (aref matrix i i))))
            ;; A_{k, i} = 0 for all k < i
            (loop for k from i below m
                  do (decf (aref matrix k j) (* q (aref matrix k i))))
            (dotimes (k n)
              (decf (aref u k j) (* q (aref u k i)))))))
      (values matrix u))))

(declaim (inline %hnf-fast!))
(defun %hnf-fast! (matrix)
  "Returns the hermite normal form H of the given m * n matrix such that m <=
n. This function destructively modified MATRIX.

NOTE: The given matrix must have full row rank. Otherwise the behaviour is
undefined.

This is a polynomial algorithm that requires O(mn^2) arithmetic operations as
well as O(mn) extended Euclidean algorithm operations on numbers up to about the
size of the determinant of (some m linearly independent columns of) MATRIX."
  (declare ((array * (* *)) matrix))
  (destructuring-bind  (m n) (array-dimensions matrix)
    (declare ((integer 0 (#.array-dimension-limit)) m n))
    (assert (<= m n))
    (let ((ext (make-array (list m (+ n m)) :element-type (array-element-type matrix) :initial-element 0)))
      (dotimes (i m)
        (dotimes (j n)
          (setf (aref ext i j) (aref matrix i j))))
      (let ((det (multiple-value-bind (det rank) (bareiss! matrix)
                   (assert (= rank m))
                   (abs det))))
        (dotimes (i m)
          (setf (aref ext i (+ n i)) det))
        (dotimes (i m)
          (loop with hi = (aref ext i i)
                for j from (+ i 1) below (+ n m)
                for hj = (aref ext i j)
                unless (zerop (aref ext i j))
                do (multiple-value-bind (x y) (ext-gcd (aref ext i i) (aref ext i j))
                     (let* ((g (+ (* x (aref ext i i)) (* y (aref ext i j))))
                            (hi/g (floor (aref ext i i) g))
                            (-hj/g (- (floor (aref ext i j) g))))
                       ;; A_{k, i} = 0 for all k < i.
                       ;; mod D can't be applied to j = n + i case.
                       (setf (aref ext i i) (if (= j (+ n i)) g (mod g det))
                             (aref ext i j) 0)
                       (loop for k from (+ i 1) below m
                             do (let ((value-i (mod (+ (* (aref ext k i) x)
                                                       (* (aref ext k j) y))
                                                    det))
                                      (value-j (mod (+ (* (aref ext k i) -hj/g)
                                                       (* (aref ext k j) hi/g))
                                                    det)))
                                  (setf (aref ext k i) value-i
                                        (aref ext k j) value-j))))))
          (dotimes (j i)
            (assert (>= (aref ext i i) 0))
            (let ((q (floor (aref ext i j) (aref ext i i))))
              ;; A_{k, i} = 0 for all k < i
              (decf (aref ext i j) (* q (aref ext i i)))
              (loop for k from (+ i 1) below m
                    do (setf (aref ext k j)
                             (mod (- (aref ext k j)
                                     (* q (aref ext k i)))
                                  det)))))))
      (adjust-array ext (list m n)))))

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
