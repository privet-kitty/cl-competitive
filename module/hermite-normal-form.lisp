(defpackage :cp/hermite-normal-form
  (:use :cl :cp/ext-gcd :cp/bareiss :cp/copy-array)
  (:export #:hnf-p #:hnf-naive #:%hnf-full-rank #:hnf #:make-hnf
           #:hnf-matrix #:hnf-unimodular-matrix #:hnf-gram-schmidt
           #:gram-schmidt #:make-gram-schmidt #:%gram-schmidt
           #:gram-schmidt-matrix #:gram-schmidt-coefs #:gram-schmidt-rank #:gram-schmidt-det2
           #:gram-schmidt-basis-rows #:gram-schmidt-row-multipliers
           #:solve-integer-linear-system)
  (:documentation "Provides an algorithm to compute the column-style (i.e. lower triangular)
Hermite normal form.

Reference:
- Friedrich Eisenbrand, Integer Programming and Algorithmic Geometry of Numbers,
  in Jünger et al., eds, 50 Years of Integer Programming 1958-2008.
- Ulfar Erlingsson, Erich Kaltofen, David Musser, Generic Gram-Schmidt
  Orthogonalization by Exact Division.
- Daniele Micciancio, Lattice Algorithms and Applications (lecture
  note), https://cseweb.ucsd.edu/classes/sp14/cse206A-a/lec4.pdf
"))
(in-package :cp/hermite-normal-form)

(declaim (inline %ref))
(defun %ref (array i j)
  (the integer (aref array i j)))

(declaim (inline (setf %ref)))
(defun (setf %ref) (new-value array i j)
  (setf (the integer (aref array i j)) new-value))

;; FIXME: This is just for validating this implementation. I will discard it
;; when I believe this module is stable.
(declaim (inline %div))
(defun %div (x y)
  (declare (integer x y))
  (multiple-value-bind (quot rem) (floor x y)
    (assert (zerop rem))
    quot))

(declaim (inline hnf-naive))
(defun hnf-naive (matrix)
  "Returns the Hermite normal form H of the given m * n matrix A such that m <= n,
and returns the unimodular matrix U such that AU = H as the second value.

Actually the returned H is the same object as MATRIX, but I don't recommend that
you exploit this behaviour.

NOTE: A must have full row rank. Otherwise the behaviour is undefined.

This algorithm requires O(mn^2) arithmetic operations as well as O(mn) extended
Euclidean algorithm operations. (Potentially this can be reduced to O(m^2n), if
you don't need the unimodular matrix.) However, it does NOT mean that this is a
polynomial algorithm, because the size of the numbers that appear in the
computation may grow exponentially. For details, please see the reference above."
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
                          (hi/g (%div (%ref matrix i i) g))
                          (-hj/g (- (%div (%ref matrix i j) g))))
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

This is a polynomial algorithm that requires O(mn*min(m, n)) arithmetic
operations on numbers up to about a constant power of the lattice determinant
given by row vectors of MATRIX."
  (declare (optimize (speed 3))
           ((array * (* *)) matrix))
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
            (when (< basis-end max-dim) ; no need to check dependency beyond MAX-DIM
              (dotimes (col n)
                (setf (aref ort-vector col)
                      (%div (- (* next-det2 (the integer (aref ort-vector col)))
                               (* mu-int (%ref matrix i-row col)))
                            det2))))
            (dotimes (j (min (+ 1 basis-end) max-dim))
              (setf (aref coef-vector j)
                    (%div (- (* next-det2 (the integer (aref coef-vector j)))
                             (* mu-int (%ref coefs i-row j)))
                          det2)))))
        ;; branches depending on linear independency
        (if (or (>= basis-end max-dim)
                (every #'zerop ort-vector))
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

(defun %hnf-full-rank (matrix &optional det)
  "Returns the Hermite normal form H of the given m * n matrix such that m <= n.

You can pass DET, the determinant of m linearly independent columns of
MATRIX. If it isn't given, this function internally calculates it.

NOTE: The given matrix must have full row rank. Otherwise the behaviour is
undefined.

This is a polynomial algorithm that requires O(m^2n) arithmetic operations as
well as O(mn) extended Euclidean algorithm operations on numbers up to about the
size of the determinant of (some m linearly independent columns of) MATRIX."
  (declare (optimize (speed 3))
           ((array * (* *)) matrix)
           ((or null integer) det))
  (destructuring-bind  (m n) (array-dimensions matrix)
    (declare ((mod #.array-dimension-limit) m n))
    (assert (<= m n))
    (let ((ext (make-array (list m (+ n m))
                           :element-type (array-element-type matrix)
                           :initial-element 0))
          (det (abs (or det
                        (let ((bareiss (bareiss! (copy-array matrix))))
                          (assert (= (bareiss-rank bareiss) m))
                          (bareiss-det bareiss))))))
      (dotimes (i m)
        (dotimes (j n)
          (setf (aref ext i j) (aref matrix i j))))
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
                          (hi/g (%div (%ref ext i i) g))
                          (-hj/g (- (%div (%ref ext i j) g))))
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
                                det))))))
      (adjust-array ext (list m n)))))

(defstruct (hnf (:predicate nil))
  ;; m * n column-style Hermite normal form 
  (matrix nil :type (array * (* *)))
  ;; n * n unimodular matrix such that (original matrix) * (unimodular matrix) = HNF
  (unimodular-matrix nil :type (or null (array * (* *))))
  ;; artifact obtained during applying the Gram-Schmidt process to the given matrix 
  (gram-schmidt nil :type gram-schmidt))

(defun hnf (matrix &optional calc-unimodular-p)
  "Computes the Hermite normal form H of the given m * n matrix A.

If CALC-UNIMODULAR-P is true, this function additionally computes the unimodular
matrix such that AU = H.

This is a polynomial algorithm that requires O(mn*min(m, n)) arithmetic
operations as well as O(mn) extended Euclidean algorithm operations on numbers
up to about a constant power of the lattice determinant given by some row or
column vectors of MATRIX. When CALC-UNIMODULAR-P is true, it requires O(mn^2)
arithmetic operations instead."
  (declare (optimize (speed 3))
           ((array * (* *)) matrix))
  (destructuring-bind  (m n) (array-dimensions matrix)
    (declare ((mod #.array-dimension-limit) m n))
    (let* ((gram-schmidt (%gram-schmidt matrix))
           (basis-rows (gram-schmidt-basis-rows gram-schmidt))
           (rank (gram-schmidt-rank gram-schmidt))
           (full-rank-submatrix (make-array (list rank n) :element-type (array-element-type matrix))))
      (dotimes (i rank)
        (let ((i-row (aref basis-rows i)))
          (dotimes (col n)
            (setf (aref full-rank-submatrix i col) (aref matrix i-row col)))))
      (let* ((bareiss (bareiss! (copy-array full-rank-submatrix)))
             (extended-full-rank-submatrix
               (if calc-unimodular-p
                   (let ((cols (bareiss-cols bareiss))
                         (res (adjust-array full-rank-submatrix (list n n) :initial-element 0))
                         (row rank))
                     (assert (= (length cols) rank))
                     (dotimes (i (+ rank 1))
                       (let ((start-col (if (zerop i) 0 (+ 1 (aref cols (- i 1)))))
                             (end-col (if (= i rank) n (aref cols i))))
                         (loop for col from start-col below end-col
                               do (setf (aref res row col) 1)
                                  (incf row))))
                     (assert (= row n))
                     res)
                   full-rank-submatrix))
             (sub-hnf (%hnf-full-rank extended-full-rank-submatrix (bareiss-det bareiss)))
             (hnf (make-array (list m n)
                              :element-type (array-element-type matrix)
                              :initial-element 0))
             (coefs (gram-schmidt-coefs gram-schmidt))
             (row-magnifiers (gram-schmidt-row-multipliers gram-schmidt))
             (restored-vector (make-array n :element-type (array-element-type matrix)))
             (last-basis-index -1))
        (declare ((integer -1 #.array-dimension-limit) last-basis-index))
        (dotimes (row m)
          (let ((basis-index (position row basis-rows)))
            (if basis-index
                (progn
                  (setq last-basis-index basis-index)
                  (dotimes (col (+ 1 basis-index))
                    (setf (aref hnf row col) (aref sub-hnf basis-index col))))
                (progn
                  (fill restored-vector 0)
                  (dotimes (i (+ last-basis-index 1))
                    (let ((coef (aref coefs row i)))
                      (declare (integer coef))
                      (dotimes (col (+ i 1))
                        (incf (the integer (aref restored-vector col))
                              (* coef (%ref sub-hnf i col))))))
                  (dotimes (col (+ last-basis-index 1))
                    (setf (aref hnf row col)
                          (%div (aref restored-vector col)
                                (aref row-magnifiers row))))))))
        (if calc-unimodular-p
            (let ((u (solve-regular-linear-system! extended-full-rank-submatrix sub-hnf)))
              (assert u)
              (make-hnf :matrix hnf :unimodular-matrix u :gram-schmidt gram-schmidt))
            (make-hnf :matrix hnf :unimodular-matrix nil :gram-schmidt gram-schmidt))))))

(declaim (inline hnf-p))
(defun hnf-p (matrix)
  "Tests if MATRIX is in the column-style Hermite normal form (i.e. lower
triangular matrix), and if so, returns the row rank."
  (declare ((array * (* *)) matrix))
  (destructuring-bind  (m n) (array-dimensions matrix)
    (declare ((mod #.array-dimension-limit) m n))
    (let ((prev-pivot-row -1)
          (rank 0))
      (declare ((mod #.array-dimension-limit) rank)
               ((integer -1 (#.array-dimension-limit)) prev-pivot-row))
      (dotimes (col n)
        (let ((pivot-row (loop for row below m
                               while (zerop (%ref matrix row col))
                               finally (return row))))
          (cond ((= m pivot-row))
                ((< prev-pivot-row pivot-row)
                 ;; check the row constraint
                 (let ((pivot-elm (%ref matrix pivot-row col)))
                   (unless (and (> pivot-elm 0)
                                (loop for j below col
                                      always (< -1 (%ref matrix pivot-row j) pivot-elm)))
                     (return-from hnf-p))
                   (incf rank)))
                (t (return-from hnf-p)))
          (setq prev-pivot-row pivot-row)))
      rank)))

(defun solve-integer-linear-system (a b)
  "Receives an m * n matrix A and an m-dimensional vector b, this function solves
Ax = b in the integer space.

If Ax = b is feasible, this function returns three values: an n-dimensional
vector c, an n * (n - rank) matrix M, and the HNF of A such that x = c + Mz is a
solution of Ax = b for any integer vector z. If it is infeasible, this function
returns (VALUES NIL NIL HNF)."
  (declare (optimize (speed 3))
           ((array * (* *)) a)
           ((array * (*)) b))
  (destructuring-bind (m n) (array-dimensions a)
    (declare ((mod #.array-dimension-limit) m n))
    (assert (= m (length b)))
    (let* ((hnf (hnf a t))
           (h (hnf-matrix hnf)) ; AU = H
           (u (hnf-unimodular-matrix hnf))
           (rank (gram-schmidt-rank (hnf-gram-schmidt hnf)))
           (y (make-array n :element-type (array-element-type a) :initial-element 1))
           (pivot-rows (make-array rank :element-type '(integer 0 #.most-positive-fixnum))))
      (let ((row 0))
        (dotimes (col rank)
          (loop while (zerop (%ref h row col))
                do (incf row))
          (setf (aref pivot-rows col) row)))
      ;; solve Hy = b
      (dotimes (row (if (zerop rank) m (aref pivot-rows 0)))
        (unless (zerop (the integer (aref b row)))
          (return-from solve-integer-linear-system (values nil nil hnf))))
      (dotimes (col rank)
        (let ((pivot-row (aref pivot-rows col)))
          (multiple-value-bind (quot rem)
              (floor (- (the integer (aref b pivot-row))
                        (loop for j below col
                              sum (* (%ref h pivot-row j) (the integer (aref y j)))
                              of-type integer))
                     (%ref h pivot-row col))
            (unless (zerop rem)
              (return-from solve-integer-linear-system (values nil nil hnf)))
            (setf (aref y col) quot))
          ;; check linearly dependent rows
          (let ((end-row (if (= (+ col 1) rank)
                             m
                             (aref pivot-rows (+ col 1)))))
            (loop for row from (+ pivot-row 1) below end-row
                  for val = (loop for j to col
                                  sum (* (%ref h row j) (the integer (aref y j)))
                                  of-type integer)
                  unless (= val (the integer (aref b row)))
                  do (return-from solve-integer-linear-system (values nil nil hnf))))))
      ;; x = Uy
      (let ((intercepts (make-array n :element-type (array-element-type a)))
            (coefs (make-array (list n (- n rank)) :element-type (array-element-type a))))
        (dotimes (i n)
          (setf (aref intercepts i)
                (loop for j below rank
                      sum (* (%ref u i j) (the integer (aref y j)))
                      of-type integer))
          (loop for j from rank below n
                do (setf (aref coefs i (- j rank)) (aref u i j))))
        (values intercepts coefs hnf)))))
