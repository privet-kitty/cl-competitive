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

(declaim (inline %ref))
(defun %ref (array i j)
  (the integer (aref array i j)))

(declaim (inline (setf %ref)))
(defun (setf %ref) (new-value array i j)
  (setf (the integer (aref array i j)) new-value))

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
Euclidean algorithm operations. (Potentially this can be reduced to O(m^2n), if
you don't need the unimodular matrix.) However, it does NOT mean that this is a
polynomial algorithm, because the size of the numbers that appear in the
computation may grow exponentially. For details, please see the reference."
  (declare ((array * (* *)) matrix))
  (destructuring-bind  (m n) (array-dimensions matrix)
    (declare ((integer 0 (#.array-dimension-limit)) m n))
    (assert (<= m n))
    (let ((u (make-array (list n n) :element-type (array-element-type matrix) :initial-element 0)))
      (dotimes (i n)
        (setf (aref u i i) 1))
      (dotimes (i m)
        (loop with hi = (%ref matrix i i)
              for j from (+ i 1) below n
              for hj = (%ref matrix i j)
              unless (zerop (%ref matrix i j))
              do (multiple-value-bind (x y) (ext-gcd (%ref matrix i i) (%ref matrix i j))
                   (let* ((g (+ (* x (%ref matrix i i)) (* y (%ref matrix i j))))
                          (hi/g (floor (%ref matrix i i) g))
                          (-hj/g (- (floor (%ref matrix i j) g))))
                     ;; A_{k, j} = 0 for all k < i
                     (loop for k from i below m
                           for value-i = (+ (* (%ref matrix k i) x)
                                            (* (%ref matrix k j) y))
                           for value-j = (+ (* (%ref matrix k i) -hj/g)
                                            (* (%ref matrix k j) hi/g))
                           do (setf (aref matrix k i) value-i
                                    (aref matrix k j) value-j))
                     (loop for k below n
                           for value-i = (+ (* (%ref u k i) x)
                                            (* (%ref u k j) y))
                           for value-j = (+ (* (%ref u k i) -hj/g)
                                            (* (%ref u k j) hi/g))
                           do (setf (aref u k i) value-i
                                    (aref u k j) value-j)))))
        (when (< (%ref matrix i i) 0)
          (dotimes (k m)
            (setf (aref matrix k i) (- (%ref matrix k i))))
          (dotimes (k n)
            (setf (aref u k i) (- (%ref u k i)))))
        (dotimes (j i)
          (let ((q (floor (%ref matrix i j) (%ref matrix i i))))
            ;; A_{k, i} = 0 for all k < i
            (loop for k from i below m
                  do (decf (%ref matrix k j) (* q (%ref matrix k i))))
            (dotimes (k n)
              (decf (%ref u k j) (* q (%ref u k i)))))))
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
          (loop with hi = (%ref ext i i)
                for j from (+ i 1) below (+ n m)
                for hj = (%ref ext i j)
                unless (zerop (%ref ext i j))
                do (multiple-value-bind (x y) (ext-gcd (%ref ext i i) (%ref ext i j))
                     (let* ((g (+ (* x (%ref ext i i))
                                  (* y (%ref ext i j))))
                            (hi/g (floor (%ref ext i i) g))
                            (-hj/g (- (floor (%ref ext i j) g))))
                       ;; A_{k, i} = 0 for all k < i.
                       ;; mod D can't be applied to j = n + i case.
                       (setf (aref ext i i) (if (= j (+ n i)) g (mod g det))
                             (aref ext i j) 0)
                       (loop for k from (+ i 1) below m
                             for value-i-old = (%ref ext k i)
                             for value-j-old = (%ref ext k j)
                             for value-i-new = (mod (+ (* value-i-old x)
                                                       (* value-j-old y))
                                                    det)
                             for value-j-new = (mod (+ (* value-i-old -hj/g)
                                                       (* value-j-old hi/g))
                                                    det)
                             do (setf (aref ext k i) value-i-new
                                      (aref ext k j) value-j-new)))))
          (dotimes (j i)
            (let ((q (floor (%ref ext i j) (%ref ext i i))))
              ;; A_{k, i} = 0 for all k < i
              (decf (%ref ext i j) (* q (%ref ext i i)))
              (loop for k from (+ i 1) below m
                    do (setf (aref ext k j)
                             (mod (- (%ref ext k j) (* q (%ref ext k i)))
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
          for diag-elm = (%ref matrix i i)
          always (and (> diag-elm 0)
                      (loop for j below i
                            always (< -1 (%ref matrix i j) diag-elm))
                      (loop for j from (+ i 1) below n
                            always (zerop (%ref matrix i j)))))))
