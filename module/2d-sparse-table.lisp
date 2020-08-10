;;;
;;; 2D sparse table
;;;
;;; Reference:
;;; https://codeforces.com/blog/entry/45485
;;;

(defpackage :cp/2d-sparse-table
  (:use :cl)
  (:export #:make-2d-sparse-table #:2dst-query))
(in-package :cp/2d-sparse-table)

(declaim (inline make-2d-sparse-table))
(defun make-2d-sparse-table (matrix binop)
  (destructuring-bind (m n) (array-dimensions matrix)
    (declare ((mod #.array-total-size-limit) m n))
    (let* ((logm (max 0 (- (integer-length (- m 1)) 1)))
           (logn (max 0 (- (integer-length (- n 1)) 1)))
           (table (make-array (list (+ logm 1) (+ logn 1) m n)
                              :element-type (array-element-type matrix))))
      (dotimes (row m)
        (dotimes (col n)
          (setf (aref table 0 0 row col) (aref matrix row col)))
        (loop
          for col-depth from 1 to logn
          for width = (ash 1 (- col-depth 1))
          do (loop
               for col below n
               do (setf (aref table 0 col-depth row col)
                        (funcall binop
                                 (aref table
                                       0 (- col-depth 1)
                                       row col)
                                 (aref table
                                       0 (- col-depth 1)
                                       row (min (- n 1) (+ col width))))))))
      (loop
        for row-depth from 1 to logm
        for width = (ash 1 (- row-depth 1))
        do (dotimes (row m)
             (loop
               for col-depth to logn
               do (dotimes (col n)
                    (setf (aref table row-depth col-depth row col)
                          (funcall binop
                                   (aref table
                                         (- row-depth 1) col-depth
                                         row col)
                                   (aref table
                                         (- row-depth 1) col-depth
                                         (min (- m 1) (+ row width)) col)))))))
      table)))

(declaim (inline 2dst-query))
(defun 2dst-query (table binop i1 j1 i2 j2 &optional identity)
  "Queries the rectangle region [I1, I2) * [J1, J2). Returns IDENTITY if the
specified region is empty."
  (declare ((integer 0 #.most-positive-fixnum) i1 j1 i2 j2))
  (assert (and (<= i1 i2 (array-dimension table 2))
               (<= j1 j2 (array-dimension table 3))))
  (if (or (= i1 i2) (= j1 j2))
      identity
      (let* ((row-depth (max 0 (- (integer-length (- i2 i1 1)) 1)))
             (col-depth (max 0 (- (integer-length (- j2 j1 1)) 1)))
             (imid (- i2 (ash 1 row-depth)))
             (jmid (- j2 (ash 1 col-depth))))
        (funcall binop
                 (funcall binop
                          (aref table row-depth col-depth i1 j1)
                          (aref table row-depth col-depth i1 jmid))
                 (funcall binop
                          (aref table row-depth col-depth imid j1)
                          (aref table row-depth col-depth imid jmid))))))
