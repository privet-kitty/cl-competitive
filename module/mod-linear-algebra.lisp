;;;
;;; Linear algebra and modular arithmetic
;;;

(defpackage :cp/mod-linear-algebra
  (:use :cl #:cp/mod-inverse)
  (:export #:mod-echelon! #:mod-determinant! #:mod-inverse-matrix! #:mod-solve-linear-system))
(in-package :cp/mod-linear-algebra)

;; Reference: http://drken1215.hatenablog.com/entry/2019/03/20/202800 (Japanese)
(declaim (inline mod-echelon!))
(defun mod-echelon! (matrix modulus &optional extended)
  "Returns the row echelon form of MATRIX by gaussian elimination and returns
the rank as the second value.

This function destructively modifies MATRIX."
  (declare ((integer 1 #.most-positive-fixnum) modulus))
  (destructuring-bind (m n) (array-dimensions matrix)
    (declare ((integer 0 #.most-positive-fixnum) m n))
    (dotimes (i m)
      (dotimes (j n)
        (setf (aref matrix i j) (mod (aref matrix i j) modulus))))
    (let ((rank 0))
      (dotimes (target-col (if extended (- n 1) n))
        (let ((pivot-row (do ((i rank (+ 1 i)))
                             ((= i m) -1)
                           (unless (zerop (aref matrix i target-col))
                             (return i)))))
          (when (>= pivot-row 0)
            ;; swap rows
            (loop for j from target-col below n
                  do (rotatef (aref matrix rank j) (aref matrix pivot-row j)))
            (let ((inv (mod-inverse (aref matrix rank target-col) modulus)))
              (dotimes (j n)
                (setf (aref matrix rank j)
                      (mod  (* inv (aref matrix rank j)) modulus)))
              (dotimes (i m)
                (unless (or (= i rank) (zerop (aref matrix i target-col)))
                  (let ((factor (aref matrix i target-col)))
                    (loop for j from target-col below n
                          do (setf (aref matrix i j)
                                   (mod (- (aref matrix i j)
                                           (mod (* (aref matrix rank j) factor) modulus))
                                        modulus)))))))
            (incf rank))))
      (values matrix rank))))

;; not tested
;; TODO: integrate into MOD-ECHELON!
(declaim (inline mod-determinant!))
(defun mod-determinant! (matrix modulus)
  "Returns the determinant of MATRIX. This function destructively modifies
MATRIX."
  (declare ((integer 1 #.most-positive-fixnum) modulus))
  (let ((n (array-dimension matrix 0)))
    (assert (= n (array-dimension matrix 1)))
    (dotimes (i n)
      (dotimes (j n)
        (setf (aref matrix i j) (mod (aref matrix i j) modulus))))
    (let ((rank 0)
          (total-product 1))
      (declare ((integer 0 #.most-positive-fixnum) rank total-product))
      (dotimes (target-col n)
        (let ((pivot-row (do ((i rank (+ 1 i)))
                             ((= i n) -1)
                           (unless (zerop (aref matrix i target-col))
                             (return i)))))
          (when (>= pivot-row 0)
            ;; swap rows
            (unless (= rank pivot-row)
              (setq total-product (mod (- total-product) modulus)))
            (loop for j from target-col below n
                  do (rotatef (aref matrix rank j) (aref matrix pivot-row j)))
            (let* ((pivot (aref matrix rank target-col))
                   (inv (mod-inverse pivot modulus)))
              (setq total-product
                    (mod (* total-product pivot) modulus))
              (dotimes (j n)
                (setf (aref matrix rank j)
                      (mod  (* inv (aref matrix rank j)) modulus)))
              (dotimes (i n)
                (unless (or (= i rank) (zerop (aref matrix i target-col)))
                  (let ((factor (aref matrix i target-col)))
                    (loop for j from target-col below n
                          do (setf (aref matrix i j)
                                   (mod (- (aref matrix i j)
                                           (mod (* (aref matrix rank j) factor) modulus))
                                        modulus)))))))
            (incf rank))))
      (let ((diag 1))
        (declare ((integer 0 #.most-positive-fixnum) diag))
        (dotimes (i n)
          (setq diag (mod (* diag (aref matrix i i)) modulus)))
        (values (mod (* diag total-product) modulus)
                rank)))))

(declaim (inline mod-inverse-matrix!))
(defun mod-inverse-matrix! (matrix modulus)
  "Returns the inverse of MATRIX by gaussian elimination if it exists and
returns NIL otherwise. This function destructively modifies MATRIX."
  (declare ((integer 1 #.most-positive-fixnum) modulus))
  (destructuring-bind (m n) (array-dimensions matrix)
    (declare ((integer 0 #.most-positive-fixnum) m n))
    (assert (= m n))
    (dotimes (i n)
      (dotimes (j n)
        (setf (aref matrix i j) (mod (aref matrix i j) modulus))))
    (let ((result (make-array (list n n) :element-type (array-element-type matrix))))
      (dotimes (i n) (setf (aref result i i) 1))
      (dotimes (target n)
        (let ((pivot-row (do ((i target (+ 1 i)))
                             ((= i n) -1)
                           (unless (zerop (aref matrix i target))
                             (return i)))))
          (when (= pivot-row -1) ; when singular
            (return-from mod-inverse-matrix! nil))
          (loop for j from target below n
                do (rotatef (aref matrix target j) (aref matrix pivot-row j))
                   (rotatef (aref result target j) (aref result pivot-row j)))
          (let ((inv (mod-inverse (aref matrix target target) modulus)))
            ;; process the pivot row
            (dotimes (j n)
              (setf (aref matrix target j)
                    (mod  (* inv (aref matrix target j)) modulus))
              (setf (aref result target j)
                    (mod  (* inv (aref result target j)) modulus)))
            ;; eliminate the column
            (dotimes (i n)
              (unless (or (= i target) (zerop (aref matrix i target)))
                (let ((factor (aref matrix i target)))
                  (dotimes (j n)
                    (setf (aref matrix i j)
                          (mod (- (aref matrix i j)
                                  (mod (* (aref matrix target j) factor) modulus))
                               modulus))
                    (setf (aref result i j)
                          (mod (- (aref result i j)
                                  (mod (* (aref result target j) factor) modulus))
                               modulus)))))))))
      result)))

(declaim (inline mod-solve-linear-system))
(defun mod-solve-linear-system (matrix vector modulus)
  "Solves Ax ≡ b and returns a root vector if it exists. Otherwise it returns
NIL. In addition, this function returns the rank of A as the second value."
  (destructuring-bind (m n) (array-dimensions matrix)
    (declare ((integer 0 #.most-positive-fixnum) m n))
    (assert (= n (length vector)))
    (let ((extended (make-array (list m (+ n 1)) :element-type (array-element-type matrix))))
      (dotimes (i m)
        (dotimes (j n) (setf (aref extended i j) (aref matrix i j)))
        (setf (aref extended i n) (aref vector i)))
      (let ((rank (nth-value 1 (mod-echelon! extended modulus t))))
        (if (loop for i from rank below m
                  always (zerop (aref extended i n)))
            (let ((result (make-array m
                                      :element-type (array-element-type matrix)
                                      :initial-element 0)))
              (dotimes (i rank)
                (setf (aref result i) (aref extended i n)))
              (values result rank))
            (values nil rank))))))
