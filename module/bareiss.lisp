(defpackage :cp/bareiss
  (:use :cl)
  (:export #:bareiss! #:bareiss #:make-bareiss #:bareiss-matrix
           #:bareiss-ext #:bareiss-rank #:bareiss-det #:bareiss-cols
           #:%solve-nonsingular-linear-system!)
  (:documentation "Provides Bareiss algorithm, a fraction-free polynomial-time
algorithm for Gaussian elimination."))
(in-package :cp/bareiss)

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

(defstruct bareiss
  ;; upper triangular matrix
  (matrix nil :type (array * (* *)))
  ;; extra matrix for which the same elementary row operations have been applied
  (ext nil :type (or null (array * (* *))))
  (rank nil :type (mod #.array-dimension-limit))
  (det nil :type integer)
  ;; linearly independent columns that were used to compute the determinant
  (cols nil :type (simple-array (integer 0 #.most-positive-fixnum) (*))))

;; TODO: two-step Bareiss algorithm should be faster
(declaim (ftype (function * (values bareiss &optional)) bareiss!))
(defun bareiss! (matrix &optional ext)
  "Receives an m * n integer matrix and transforms it to an upper triangular form
by elementary row operations.

If an m * <any width> matrix EXT is given, the same operations are applied to
EXT, which is used to solve linear equations like MATRIX * X = EXT.

This function requires O(mn*min(m, n)) arithmetic operations on numbers up to
about the determinant (or the Hadamard bound, equivalently).

This is a so-called single-step Bareiss algorithm. For details, please see the
reference.

NOTE: When the MATRIX is not square, the computed DET means the determinant of
the min(m, n) linearly independent rows or columns. (The latter is stored in
COLS.)

NOTE: NOT SUFFICIENTLY TESTED for non-row-full-rank case!!

Reference:
Erwin H. Bareiss. Sylvester's Identity and Multistep Integer-Preserving Gaussian
Elimination. (1967)"
  (declare (optimize (speed 3))
           ((array * (* *)) matrix)
           ((or null (array * (* *))) ext))
  (destructuring-bind (m n) (array-dimensions matrix)
    (declare ((mod #.array-dimension-limit) m n))
    (let ((ext-n (when ext
                   (assert (= m (array-dimension ext 0)))
                   (array-dimension ext 1))))
      (declare ((or null (mod #.array-dimension-limit)) ext-n))
      (let ((prev-diag 1)
            (cols (make-array m :element-type '(integer 0 #.most-positive-fixnum)))
            (target-col 0)
            (sign 1))
        (declare ((integer -1 1) sign)
                 ((mod #.array-dimension-limit) target-col)
                 (integer prev-diag))
        (labels ((%return (end-row)
                   (loop for i from end-row below m
                         do (dotimes (j n)
                              (setf (aref matrix i j) 0)))
                   (return-from bareiss!
                     (make-bareiss :matrix matrix
                                   :ext ext
                                   :rank end-row
                                   :det (if (= end-row (min m n))
                                            (* sign prev-diag)
                                            0)
                                   :cols (subseq cols 0 end-row)))))
          (dotimes (k (min m n) (%return k))
            (loop
              ;; use a non-zero entry on the target column as a pivot 
              (when (= target-col n)
                (%return k))
              (let ((nz-row (loop for i from k below m
                                  unless (zerop (%ref matrix i target-col))
                                  do (return i))))
                (when nz-row
                  (unless (= k nz-row)
                    (dotimes (j n)
                      (rotatef (aref matrix k j) (aref matrix nz-row j)))
                    (when ext
                      (dotimes (j ext-n)
                        (rotatef (aref ext k j) (aref ext nz-row j))))
                    (setq sign (- sign)))
                  (setf (aref cols k) target-col)
                  (return)))
              (incf target-col))
            (dotimes (j k)
              (setf (aref matrix k j) 0))
            (let ((diag (%ref matrix k target-col)))
              (loop for i from (+ k 1) below m
                    do (loop for j from (+ target-col 1) below n
                             do (setf (aref matrix i j)
                                      (%div (- (* (%ref matrix i j) diag)
                                               (* (%ref matrix i target-col) (%ref matrix k j)))
                                            prev-diag)))
                       (when ext-n
                         (dotimes (j ext-n)
                           (setf (aref ext i j)
                                 (%div (- (* (%ref ext i j) diag)
                                          (* (%ref matrix i target-col) (%ref ext k j)))
                                       prev-diag)))))
              (setq prev-diag diag))
            (incf target-col)))))))

(defun %solve-nonsingular-linear-system! (a b)
  "Given an m * m nonsingular matrix A and an m * n matrix B, this function returns
the m * n (integer) matrix X such that AX = B if it exists. Otherwise it returns
NIL.

In addition, it returns the BAREISS sturucture of A as the second value."
  (declare (optimize (speed 3))
           ((array * (* *)) a b))
  (destructuring-bind (m n) (array-dimensions b)
    (declare ((mod #.array-dimension-limit) m n))
    (assert (= m (array-dimension a 0) (array-dimension a 1)))
    (let* ((bareiss (bareiss! a b))
           (a-trans (bareiss-matrix bareiss))
           (b-trans (bareiss-ext bareiss))
           (res (make-array (list m n) :element-type (array-element-type a))))
      (assert (= (bareiss-rank bareiss) m))
      (loop for i from (- m 1) downto 0
            do (dotimes (j n)
                 (let ((val (%ref b-trans i j)))
                   (declare (integer val))
                   (loop for k from (+ i 1) below m
                         do (decf val (* (%ref a-trans i k) (%ref res k j))))
                   (multiple-value-bind (quot rem) (floor val (%ref a-trans i i))
                     (unless (zerop rem)
                       (return-from %solve-nonsingular-linear-system!
                         (values nil bareiss)))
                     (setf (aref res i j) quot)))))
      (values res bareiss))))
