(defpackage :cp/two-phase-simplex
  (:use :cl)
  (:export #:dual-primal!)
  (:documentation "Provides two phase simplex method. (dual-primal)

Reference:
Robert J. Vanderbei. Linear Programming: Foundations and Extensions. 5th edition."))
(in-package :cp/two-phase-simplex)

;; NOTE: not optimized at all

(defconstant +eps+ 1d-8)

(defconstant +neg-inf+ most-negative-double-float)
(defconstant +pos-inf+ most-positive-double-float)

;; dict: col(0), col(1), ...., col(n-1), row(0), row(1), ..., row(m-1)
;;      |---------- non-basic ---------| |---------- basic ----------|

(declaim (ftype (function * (values double-float &optional)) %pivot))
(defun %pivot (row col a b c arow acol dict)
  (declare (optimize (speed 3))
           ((mod #.array-dimension-limit) row col)
           ((simple-array double-float (* *)) a)
           ((simple-array double-float (*)) b c arow acol)
           ((simple-array fixnum (*)) dict))
  (destructuring-bind (m n) (array-dimensions a)
    (declare ((mod #.array-dimension-limit) m n))
    (rotatef (aref dict col) (aref dict (+ n row)))
    (let* ((apivot (aref a row col))
           (/apivot (/ apivot)))
      (dotimes (i m)
        (dotimes (j n)
          (decf (aref a i j) (* (aref acol i) (aref arow j) /apivot))))
      (dotimes (j n)
        (setf (aref a row j) (* (aref arow j) /apivot)))
      (dotimes (i m)
        (setf (aref a i col) (- (* (aref acol i) /apivot))))
      (setf (aref a row col) /apivot)
      (let ((brow (aref b row)))
        (dotimes (i m)
          (decf (aref b i) (* brow (aref acol i) /apivot)))
        (setf (aref b row) (* brow /apivot))
        (let ((ccol (aref c col)))
          (dotimes (j n)
            (decf (aref c j) (* ccol (aref arow j) /apivot)))
          (setf (aref c col) (- (* ccol /apivot)))
          (* ccol brow /apivot))))))

(defun %restore (b c dict)
  (declare (optimize (speed 3))
           ((simple-array double-float (*)) b c)
           ((simple-array fixnum (*)) dict))
  (let* ((m (length b))
         (n (length c))
         (res-primal (make-array n :element-type 'double-float :initial-element 0d0))
         (res-dual (make-array m :element-type 'double-float :initial-element 0d0)))
    (dotimes (i m)
      (let ((index (aref dict (+ n i))))
        (when (< index n)
          (setf (aref res-primal index) (aref b i)))))
    (dotimes (j n)
      (let ((index (aref dict j)))
        (when (>= index n)
          (setf (aref res-dual (- index n)) (- (aref c j))))))
    (values res-primal res-dual)))

(declaim (ftype (function * (values (simple-array fixnum (*)) &optional))
                %iota))
(defun %iota (n)
  (let ((res (make-array n :element-type 'fixnum :initial-element 0)))
    (dotimes (i n)
      (setf (aref res i) i))
    res))

(defun %primal-simplex! (a b c &optional dict)
  "Assumes b >= 0."
  (declare (optimize (speed 3))
           ((simple-array double-float (* *)) a)
           ((simple-array double-float (*)) b c)
           ((or null (simple-array fixnum (*))) dict))
  (destructuring-bind (m n) (array-dimensions a)
    (declare ((mod #.array-dimension-limit) m n))
    (let ((acol (make-array m :element-type 'double-float))
          (arow (make-array n :element-type 'double-float))
          (dict (or dict (%iota (+ m n))))
          (obj 0d0))
      (declare (double-float obj))
      (loop
        ;; pick largest coefficient
        (let (col
              (colmax +neg-inf+))
          (dotimes (j n)
            (when (> (aref c j) colmax)
              (setq colmax (aref c j)
                    col j)))
          (unless col
            (error "All coefficient ~A are too small." c))
          (when (< colmax +eps+)
            (return))
          (dotimes (i m)
            (setf (aref acol i) (aref a i col)))
          ;; select leaving variable
          (unless (find-if (lambda (x) (> x +eps+)) acol)
            (return-from %primal-simplex! (values :unbounded nil nil nil)))
          (let (row
                (rowmin +pos-inf+))
            (dotimes (i m)
              (when (> (aref acol i) +eps+)
                (let ((rate (/ (aref b i) (aref acol i))))
                  (when (< rate rowmin)
                    (setq row i
                          rowmin rate)))))
            (unless row
              (error "Pivot not found in column ~A." acol))
            (dotimes (j n)
              (setf (aref arow j) (aref a row j)))
            ;; pivot
            (incf obj (%pivot row col a b c arow acol dict)))))
      ;; restore primal & dual solutions
      (multiple-value-bind (res-primal res-dual) (%restore b c dict)
        (values obj res-primal res-dual dict)))))

(defun %dual-simplex! (a b c &optional dict)
  "Assumes c <= 0."
  (declare (optimize (speed 3))
           ((simple-array double-float (* *)) a)
           ((simple-array double-float (*)) b c)
           ((or null (simple-array fixnum (*))) dict))
  (destructuring-bind (m n) (array-dimensions a)
    (declare ((mod #.array-dimension-limit) m n))
    (let ((acol (make-array m :element-type 'double-float))
          (arow (make-array n :element-type 'double-float))
          (dict (or dict (%iota (+ m n))))
          (obj 0d0))
      (declare (double-float obj))
      (loop
        ;; pick least intercept
        (let (row
              (rowmin +pos-inf+))
          (dotimes (i m)
            (when (< (aref b i) rowmin)
              (setq rowmin (aref b i)
                    row i)))
          (unless row
            (error "All intercepts ~A are too large." b))
          (when (> rowmin +eps+)
            (return))
          (dotimes (j n)
            (setf (aref arow j) (aref a row j)))
          ;; select leaving variable
          (unless (find-if (lambda (x) (< x (- +eps+))) arow)
            (return-from %dual-simplex! (values :infeasible nil nil nil)))
          (let (col
                (colmin +pos-inf+))
            (dotimes (j n)
              (when (< (aref arow j) (- +eps+))
                (let ((rate (/ (aref c j) (aref arow j))))
                  (when (< rate colmin)
                    (setq col j
                          colmin rate)))))
            (unless col
              (error "Pivot not found in row ~A." arow))
            (dotimes (i m)
              (setf (aref acol i) (aref a i col)))
            (incf obj (%pivot row col a b c arow acol dict)))))
      ;; restore primal & dual solutions
      (multiple-value-bind (res-primal res-dual) (%restore b c dict)
        (values obj res-primal res-dual dict)))))

(declaim (ftype (function * (values (or double-float (member :unbounded :infeasible))
                                    (or null (simple-array double-float (*)))
                                    (or null (simple-array double-float (*)))
                                    (or null (simple-array fixnum (*)))
                                    &optional))
                dual-primal!))
(defun dual-primal! (a b c)
  "Maximizes cx subject to Ax <= b and x >= 0. Returns four values:

Optimal case:
- optimal objective value
- solutions for primal problem
- solutions for dual problem: min. by s.t. (A^t)y >= c and y >= 0
- current dictionary

Unbounded case:
- (values :unbounded nil nil nil)

Infeasible case:
- (values :infeasible nil nil nil)"
  (declare (optimize (speed 3))
           ((simple-array double-float (* *)) a)
           ((simple-array double-float (*)) b c))
  (destructuring-bind (m n) (array-dimensions a)
    (declare ((mod #.array-dimension-limit) m n))
    ;; Phase I: solve modified problem with objective = -x1-x2- ... -xn
    (let* ((c* (make-array n :element-type 'double-float :initial-element -1d0))
           (dict (%iota (+ n m)))
           (result1 (%dual-simplex! a b c* dict)))
      (when (eql result1 :infeasible)
        (return-from dual-primal! (values :infeasible nil nil nil)))
      ;; restore original objective function
      (let ((poses (make-array (+ n m) :element-type 'fixnum)))
        (dotimes (i (+ m n))
          (setf (aref poses (aref dict i)) i))
        (replace c* c)
        (fill c 0d0)
        (let ((obj 0d0))
          (declare (double-float obj))
          (dotimes (j n)
            (let* ((coef (aref c* j))
                   (pos (aref poses j)))
              (if (< pos n)
                  ;; xj is non-basic
                  (let ((col pos))
                    (incf (aref c col) coef))
                  ;; xj is basic
                  (let ((row (- pos n)))
                    (dotimes (j n)
                      (decf (aref c j) (* coef (aref a row j))))
                    (incf obj (* coef (aref b row)))))))
          ;; Phase II: solve original problem
          (multiple-value-bind (result2 primal dual) (%primal-simplex! a b c dict)
            (if (eql result2 :unbounded)
                (values result2 nil nil nil)
                (values (+ obj (the double-float result2)) primal dual dict))))))))
