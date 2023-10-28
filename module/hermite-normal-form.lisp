(defpackage :cp/hermite-normal-form
  (:use :cl :cp/ext-gcd :cp/bareiss :cp/copy-array)
  (:import-from :cp/bareiss #:bareiss!)
  (:export #:hnf-naive #:%hnf-full-rank #:hnf #:hnf-p
           #:gram-schmidt #:make-gram-schmidt #:%gram-schmidt
           #:gram-schmidt-matrix #:gram-schmidt-coefs #:gram-schmidt-rank #:gram-schmidt-det2
           #:gram-schmidt-basis-rows #:gram-schmidt-row-multipliers)
  (:documentation "Provides an algorithm to compute the Hermite normal form.

Reference:
- Friedrich Eisenbrand, Integer Programming and Algorithmic Geometry of Numbers,
  in Jünger et al., eds, 50 Years of Integer Programming 1958-2008.
- Ulfar Erlingsson, Erich Kaltofen, David Musser, Generic Gram-Schmidt
  Orthogonalization by Exact Division.
- Daniele Micciancio, Lattice Algorithms and Applications (lecture
  note), https://cseweb.ucsd.edu/classes/sp14/cse206A-a/lec4.pdf
"))
(in-package :cp/hermite-normal-form)

;; **WORK IN PROGRESS**

;; TODO: It would be better to have the unimodular matrix as a sequence of
;; operations than to have it as an n * n matrix, especially when n is large.

(declaim (inline %ref))
(defun %ref (array i j)
  (the integer (aref array i j)))

(declaim (inline (setf %ref)))
(defun (setf %ref) (new-value array i j)
  (setf (the integer (aref array i j)) new-value))

(declaim (inline hnf-naive))
(defun hnf-naive (matrix)
  "Returns the hermite normal form H of the given m * n matrix A such that m <= n,
and returns the unimodular matrix U such that AU = H as the second value.

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
    (declare ((mod #.array-dimension-limit) m n))
    (assert (<= m n))
    (let* ((matrix (copy-array matrix))
           (u (make-array (list n n) :element-type (array-element-type matrix) :initial-element 0)))
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

(declaim (inline %div))
(defun %div (x y)
  (declare (integer x y))
  (multiple-value-bind (quot rem) (floor x y)
    (assert (zerop rem))
    quot))

;; TODO: Can I further decrease the magnitute of the numbers that appear in the
;; computation?
(defstruct gram-schmidt
  "Let b*_1, ..., b*_k be the basis produced by the Gram-Schmidt algorithm from
vectors b_1, ..., b_m (which is given as the row vectors in an m * n
matrix). This structure stores the `integerized' orthogonal vectors b'_1, ...,
b'_k such that b'_i = D_i * b*_i for some integer scalar D_i."
  ;; MATRIX[i] := an integerized orthogonalized vector b'_i of the original row
  ;; vector b_i if i is contained in BASIS-ROWS, otherwise a zero vector.
  (matrix nil :type (array * (* *)))
  ;; COEFS[i] := the coefficients of the integer linear representation of
  ;; MATRIX[i] (i.e. b'_i) by the preceding given vectors b_0, ..., b_{i-1} if
  ;; it is contained in BASIS-ROWS, otherwise that of b_i by the preceding given
  ;; vectors as well. This array has a size of m * rank.
  (coefs nil :type (array * (* *)))
  (rank nil :type (mod #.array-dimension-limit))
  ;; the squared determinant of the lattice spanned by the basis. (Note that it
  ;; is unique w.r.t. the basis though the basis itself isn't necessarily
  ;; unique.)
  (det2 nil :type integer)
  ;; row indices of the basis (in ascending order)
  (basis-rows nil :type (simple-array (integer 0 #.most-positive-fixnum) (*)))
  ;; This array holds an integer constant D_i that was multiplied to MATRIX[i]
  ;; and COEFS[i] in order to avoid a rational representation. In other words,
  ;; MATRIX and COEFS can be regarded as matrices of numerators and this is the
  ;; one of the denominators of the rows.
  (row-multipliers nil :type (simple-array * (*))))

(defun %gram-schmidt (matrix)
  "Applies the Gram-Schmidt algotithm to the each **row** of the given integer
matrix.

This is a polynomial algorithm that requires O(m^2n) arithmetic operations on
numbers up to about a constant multiple of the lattice determinant given by row
vectors of MATRIX."
  (declare ((array * (* *)) matrix))
  (destructuring-bind (m n) (array-dimensions matrix)
    (declare ((mod #.array-dimension-limit) m n))
    (let* ((matrix (copy-array matrix))
           (max-dim (min m n))
           (coefs (make-array (list m max-dim)
                              :element-type (array-element-type matrix)
                              :initial-element 0))
           (basis-rows (make-array max-dim :element-type '(integer 0 #.most-positive-fixnum)))
           (basis-end 0)
           (l2s (make-array max-dim :element-type (array-element-type matrix)))
           (det2s (make-array (+ 1 max-dim) :element-type (array-element-type matrix)))
           (ort-vector (make-array n :element-type (array-element-type matrix)))
           (coef-vector (make-array m :element-type (array-element-type matrix)))
           (row-multipliers (make-array m :element-type 'integer)))
      (setf (aref det2s 0) 1)
      (dotimes (row m)
        (setf (aref row-multipliers row) (aref det2s basis-end))
        (dotimes (col n)
          (setf (aref ort-vector col) (%ref matrix row col)))
        (fill coef-vector 0)
        (setf (aref coef-vector basis-end) 1)
        (dotimes (i basis-end)
          (let* ((i-row (aref basis-rows i))
                 (det2 (aref det2s i))
                 (next-det2 (aref det2s (+ i 1)))
                 (mu-int (%div (* (loop for col below n
                                        sum (* (%ref matrix row col)
                                               (%ref matrix i-row col))
                                        of-type integer)
                                  next-det2
                                  det2)
                               (aref l2s i))))
            (declare (integer det2 next-det2))
            ;; TODO: no need to check independency beyond MAX-DIM
            (dotimes (col n)
              (setf (aref ort-vector col)
                    (%div (- (* next-det2 (the integer (aref ort-vector col)))
                             (* mu-int (%ref matrix i-row col)))
                          det2)))
            (dotimes (j (min (+ 1 basis-end) max-dim))
              (setf (aref coef-vector j)
                    (%div (- (* next-det2 (the integer (aref coef-vector j)))
                             (* mu-int (%ref coefs i-row j)))
                          det2)))))
        ;; branches depending on linear independency
        (if (every #'zerop ort-vector)
            (progn
              (dotimes (col n)
                (setf (%ref matrix row col) 0))
              (dotimes (j basis-end)
                (setf (%ref coefs row j) (- (aref coef-vector j)))))
            (let* ((new-l2 (loop for col below n
                                 sum (expt (the integer (aref ort-vector col)) 2)
                                 of-type integer))
                   (new-det2 (%div new-l2 (aref det2s basis-end))))
              (setf (aref l2s basis-end) new-l2
                    (aref det2s (+ 1 basis-end)) new-det2
                    (aref basis-rows basis-end) row)
              (dotimes (col n)
                (setf (aref matrix row col) (aref ort-vector col)))
              (dotimes (j (+ 1 basis-end))
                (setf (aref coefs row j) (aref coef-vector j)))
              (incf basis-end))))
      (make-gram-schmidt :matrix matrix
                         :coefs (adjust-array coefs (list m basis-end))
                         :rank basis-end
                         :det2 (aref det2s basis-end)
                         :basis-rows (adjust-array basis-rows basis-end)
                         :row-multipliers row-multipliers))))

(declaim (inline %hnf-full-rank))
(defun %hnf-full-rank (matrix)
  "Returns the Hermite normal form H of the given m * n matrix such that m <= n.

NOTE: The given matrix must have full row rank. Otherwise the behaviour is
undefined.

This is a polynomial algorithm that requires O(m^2n) arithmetic operations as
well as O(mn) extended Euclidean algorithm operations on numbers up to about the
size of the determinant of (some m linearly independent columns of) MATRIX."
  (declare ((array * (* *)) matrix))
  (destructuring-bind  (m n) (array-dimensions matrix)
    (declare ((mod #.array-dimension-limit) m n))
    (assert (<= m n))
    (let ((matrix (copy-array matrix))
          (ext (make-array (list m (+ n m))
                           :element-type (array-element-type matrix)
                           :initial-element 0)))
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
                       ;; 1. A_{k, i} = 0 for all k < i.
                       ;; 2. mod D can't be applied to the j = n + i case.
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

(defun hnf (matrix)
  "Returns the Hermite normal form H of the given (not necessarily full row rank) m
* n matrix.

This is a polynomial algorithm that requires O(m^2n) arithmetic operations as
well as O(mn) extended Euclidean algorithm operations on numbers up to about a
constant multiple of the lattice determinant given by some row or column vectors
of MATRIX."
  (declare ((array * (* *)) matrix))
  (destructuring-bind  (m n) (array-dimensions matrix)
    (declare ((mod #.array-dimension-limit) m n))
    ;; work in progress
    ))

(declaim (inline hnf-p))
(defun hnf-p (matrix)
  "Tests if MATRIX is in the Hermite normal form.

Note that currently this function assumes that MATRIX has full row rank."
  (declare ((array * (* *)) matrix))
  (destructuring-bind  (m n) (array-dimensions matrix)
    (declare ((mod #.array-dimension-limit) m n))
    (assert (<= m n))
    (loop for i below m
          for diag-elm = (%ref matrix i i)
          always (and (> diag-elm 0)
                      (loop for j below i
                            always (< -1 (%ref matrix i j) diag-elm))
                      (loop for j from (+ i 1) below n
                            always (zerop (%ref matrix i j)))))))
