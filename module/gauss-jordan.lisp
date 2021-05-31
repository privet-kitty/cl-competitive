(defpackage :cp/gauss-jordan
  (:use :cl)
  (:export #:echelon! #:solve-linear-system))
(in-package :cp/gauss-jordan)

(deftype gauss-jordan-float () 'double-float)
(defconstant +zero+ (coerce 0 'gauss-jordan-float))
(defconstant +eps+ (coerce 1d-9 'gauss-jordan-float))

;; Reference: http://drken1215.hatenablog.com/entry/2019/03/20/202800
(declaim (ftype (function * (values (mod #.array-dimension-limit)
                                    (simple-array fixnum (*))
                                    &optional))
                echelon!))
(defun echelon! (matrix &optional extended)
  "Destructively transforms MATRIX to the row echelon form by Gaussian
elimination and returns the rank as the second value."
  (declare (optimize (speed 3))
           ((simple-array gauss-jordan-float (* *)) matrix))
  (labels ((%zerop (x) (< (abs x) +eps+)))
    (destructuring-bind (m n) (array-dimensions matrix)
      (declare ((mod #.array-dimension-limit) m n))
      (let ((rank 0)
            (cols (make-array m :element-type 'fixnum :initial-element -1)))
        (declare ((mod #.array-dimension-limit) rank))
        (dotimes (target-col (if extended (- n 1) n))
          (let ((pivot-row (do ((i rank (+ 1 i)))
                               ((= i m) -1)
                             (unless (%zerop (aref matrix i target-col))
                               (return i)))))
            (when (>= pivot-row 0)
              ;; swap rows
              (loop for j from target-col below n
                    do (rotatef (aref matrix rank j) (aref matrix pivot-row j)))
              (let ((inv (/ (aref matrix rank target-col))))
                (dotimes (j n)
                  (setf (aref matrix rank j)
                        (* inv (aref matrix rank j))))
                (dotimes (i m)
                  (unless (or (= i rank) (%zerop (aref matrix i target-col)))
                    (let ((factor (aref matrix i target-col)))
                      (loop for j from target-col below n
                            do (decf (aref matrix i j)
                                     (* (aref matrix rank j) factor)))))))
              (setf (aref cols rank) target-col)
              (incf rank))))
        (values rank cols)))))

(declaim (inline solve-linear-system))
(defun solve-linear-system (matrix vector)
  "Solves Ax = b and returns a root vector if it exists. Otherwise it returns
NIL. In addition, this function returns the rank of A as the second value."
  (declare (vector vector))
  (destructuring-bind (m n) (array-dimensions matrix)
    (declare ((mod #.array-dimension-limit) m n))
    (assert (= m (length vector)))
    (let* ((extended (make-array (list m (+ n 1)) :element-type 'gauss-jordan-float)))
      (labels ((%zerop (x) (< (abs x) +eps+)))
        (dotimes (i m)
          (dotimes (j n) (setf (aref extended i j) (aref matrix i j)))
          (setf (aref extended i n) (aref vector i)))
        (multiple-value-bind (rank cols) (echelon! extended t)
          (if (loop for i from rank below m
                    always (%zerop (aref extended i n)))
              (let ((result (make-array n
                                        :element-type 'gauss-jordan-float
                                        :initial-element +zero+)))
                (dotimes (i m)
                  (let ((j (aref cols i)))
                    (when (>= j 0)
                      (setf (aref result j) (aref extended i n)))))
                (values result rank))
              (values nil rank)))))))
