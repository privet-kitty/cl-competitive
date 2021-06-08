(defpackage :cp/two-phase-simplex
  (:use :cl)
  (:export #:dual-primal!)
  (:documentation "Provides two-phase (dual-primal) revised simplex method using
Dantzig's pivot rule.

Reference:
Robert J. Vanderbei. Linear Programming: Foundations and Extensions. 5th edition."))
(in-package :cp/two-phase-simplex)

(deftype simplex-float () 'double-float)
(defconstant +zero+ (coerce 0 'simplex-float))
(defconstant +eps+ 1d-8)
(defconstant +neg-inf+ most-negative-double-float)
(defconstant +pos-inf+ most-positive-double-float)


;; dict: col(0), col(1), ...., col(n-1), row(0), row(1), ..., row(m-1)
;;      |---------- non-basic ---------| |---------- basic ----------|

(declaim (ftype (function * (values simplex-float &optional)) %pivot))
(defun %pivot (row col a b c arow acol dict)
  (declare (optimize (speed 3))
           ((mod #.array-dimension-limit) row col)
           ((simple-array simplex-float (* *)) a)
           ((simple-array simplex-float (*)) b c arow acol)
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
           ((simple-array simplex-float (*)) b c)
           ((simple-array fixnum (*)) dict))
  (let* ((m (length b))
         (n (length c))
         (res-primal (make-array n :element-type 'simplex-float :initial-element +zero+))
         (res-dual (make-array m :element-type 'simplex-float :initial-element +zero+)))
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
  (declare ((mod #.array-dimension-limit) n))
  (let ((res (make-array n :element-type 'fixnum :initial-element 0)))
    (dotimes (i n)
      (setf (aref res i) i))
    res))

(defun %primal-simplex! (a b c &optional dict)
  "Assumes b >= 0."
  (declare (optimize (speed 3))
           ((simple-array simplex-float (* *)) a)
           ((simple-array simplex-float (*)) b c)
           ((or null (simple-array fixnum (*))) dict))
  (destructuring-bind (m n) (array-dimensions a)
    (declare ((mod #.array-dimension-limit) m n))
    (let ((acol (make-array m :element-type 'simplex-float))
          (arow (make-array n :element-type 'simplex-float))
          (dict (or dict (%iota (+ m n))))
          (obj +zero+))
      (declare (simplex-float obj))
      (loop
        ;; pick largest coefficient
        (let (col
              (colmax +neg-inf+))
          (dotimes (j n)
            (when (> (aref c j) colmax)
              (setq colmax (aref c j)
                    col j)))
          (when (or (null col) (<= colmax +eps+))
            (return))
          (dotimes (i m)
            (setf (aref acol i) (aref a i col)))
          ;; select leaving variable
          (unless (find-if (lambda (x) (> x +eps+)) acol)
            (return-from %primal-simplex! (values :unbounded nil nil)))
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
        (values obj res-primal res-dual)))))

(defun %dual-simplex! (a b c &optional dict)
  "Assumes c <= 0."
  (declare (optimize (speed 3))
           ((simple-array simplex-float (* *)) a)
           ((simple-array simplex-float (*)) b c)
           ((or null (simple-array fixnum (*))) dict))
  (destructuring-bind (m n) (array-dimensions a)
    (declare ((mod #.array-dimension-limit) m n))
    (let ((acol (make-array m :element-type 'simplex-float))
          (arow (make-array n :element-type 'simplex-float))
          (dict (or dict (%iota (+ m n))))
          (obj +zero+))
      (declare (simplex-float obj))
      (loop
        ;; pick least intercept
        (let (row
              (rowmin +pos-inf+))
          (dotimes (i m)
            (when (< (aref b i) rowmin)
              (setq rowmin (aref b i)
                    row i)))
          (when (or (null row) (>= rowmin (- +eps+)))
            (return))
          (dotimes (j n)
            (setf (aref arow j) (aref a row j)))
          ;; select leaving variable
          (unless (find-if (lambda (x) (< x (- +eps+))) arow)
            (return-from %dual-simplex! (values :infeasible nil nil)))
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
        (values obj res-primal res-dual)))))

(declaim (ftype (function * (values (or simplex-float (member :unbounded :infeasible))
                                    (or null (simple-array simplex-float (*)))
                                    (or null (simple-array simplex-float (*)))
                                    &optional))
                dual-primal!))
(defun dual-primal! (a b c &optional dict)
  "Maximizes cx subject to Ax <= b and x >= 0. Returns four values:

Optimal case:
- optimal objective value
- optimal solutions to the primal problem
- optimal solutions to the dual problem: min. by s.t. (A^t)y >= c, y >= 0
- current dictionary

Unbounded case:
- (values :unbounded nil nil nil)

Infeasible case:
- (values :infeasible nil nil nil)"
  (declare (optimize (speed 3))
           ((simple-array simplex-float (* *)) a)
           ((simple-array simplex-float (*)) b c)
           ((or null (simple-array fixnum (*))) dict))
  (destructuring-bind (m n) (array-dimensions a)
    (declare ((mod #.array-dimension-limit) m n))
    ;; Phase I: apply dual to the modified problem with objective = -x1-x2-
    ;; ... -xn
    (let* ((c* (make-array n :element-type 'simplex-float
                             :initial-element (coerce -1 'simplex-float)))
           (dict (or dict (%iota (+ n m))))
           (dict-saved (copy-seq dict))
           (result1 (%dual-simplex! a b c* dict)))
      ;; restore original objective function (even when phase I ended with :infeasible)
      (let ((poses (make-array (+ n m) :element-type 'fixnum)))
        (dotimes (i (+ m n))
          (setf (aref poses (aref dict i)) i))
        (replace c* c)
        (fill c +zero+)
        (let ((obj +zero+))
          (declare (simplex-float obj))
          (dotimes (prev-pos n)
            ;; j: position w.r.t. dict-saved
            ;; new-pos: position w.r.t. dict
            (let* ((coef (aref c* prev-pos))
                   (var (aref dict-saved prev-pos))
                   (new-pos (aref poses var)))
              (if (< new-pos n)
                  ;; xj is non-basic
                  (let ((col new-pos))
                    (incf (aref c col) coef))
                  ;; xj is basic
                  (let ((row (- new-pos n)))
                    (dotimes (j n)
                      (decf (aref c j) (* coef (aref a row j))))
                    (incf obj (* coef (aref b row)))))))
          (when (eql result1 :infeasible)
            (return-from dual-primal! (values :infeasible nil nil)))
          ;; Phase II: apply primal to the original problem
          (multiple-value-bind (result2 primal dual) (%primal-simplex! a b c dict)
            (if (eql result2 :unbounded)
                (values result2 nil nil)
                (values (+ obj (the simplex-float result2)) primal dual))))))))
