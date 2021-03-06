(defpackage :cp/self-dual-simplex
  (:use :cl :cp/simplex-common)
  (:export #:self-dual!)
  (:documentation "Provides parametric self-dual simplex method for dense LP.

Reference:
Robert J. Vanderbei. Linear Programming: Foundations and Extensions. 5th edition."))
(in-package :cp/self-dual-simplex)

(declaim (ftype (function * (values (or simplex-float (member :dual-infeasible :infeasible))
                                    (or null (simple-array simplex-float (*)))
                                    (or null (simple-array simplex-float (*)))
                                    &optional))
                self-dual!))
(defun self-dual! (a b c &optional dict)
  "Maximizes c'x subject to Ax <= b and x >= 0. Returns three values:

Optimal case:
- optimal objective value
- optimal solutions to the primal problem
- optimal solutions to the dual problem: min. b'y s.t. A'y >= c, y >= 0

Primal infeasible case:
- (values :infeasible nil nil)

Dual infeasible case:
- (values :dual-infeasible nil nil)

Note that the result could be :infeasible or :dual-infeasible when both is
infeasible. (It is not even deterministic.)"
  (declare (optimize (speed 3))
           ((simple-array simplex-float (* *)) a)
           ((simple-array simplex-float (*)) b c)
           ((or null (simple-array fixnum (*))) dict))
  (destructuring-bind (m n) (array-dimensions a)
    (declare ((mod #.array-dimension-limit) m n))
    (let ((acol (make-array m :element-type 'simplex-float))
          (arow (make-array n :element-type 'simplex-float))
          (cparams (make-array n :element-type 'simplex-float))
          (bparams (make-array m :element-type 'simplex-float))
          (dict (or dict (let ((dict (make-array (+ n m) :element-type 'fixnum)))
                           (dotimes (i (+ n m) dict)
                             (setf (aref dict i) i)))))
          (obj +zero+))
      (declare (simplex-float obj))
      (dotimes (j n)
        (setf (aref cparams j) (+ 0.5d0 (random 1d0))))
      (dotimes (i m)
        (setf (aref bparams i) (+ 0.5d0 (random 1d0))))
      (loop
        (let ((mu-lo +neg-inf+)
              index)
          (dotimes (j n)
            (when (> (aref cparams j) +eps+)
              (let ((lo (/ (aref c j) (aref cparams j))))
                (when (> lo mu-lo)
                  (setq mu-lo lo
                        index j)))))
          (dotimes (i m)
            (when (> (aref bparams i) +eps+)
              (let ((lo (/ (- (aref b i)) (aref bparams i))))
                (when (> lo mu-lo)
                  (setq mu-lo lo
                        index (+ i n))))))
          (when (<= mu-lo +eps+)
            (return))
          (if (>= index n)
              ;; dual pivotting
              (let ((row (- index n)))
                (dotimes (j n)
                  (setf (aref arow j) (aref a row j)))
                (unless (find-if (lambda (x) (< x (- +eps+))) arow)
                  (return-from self-dual! (values :infeasible nil nil)))
                (let (col
                      (colmin +pos-inf+))
                  (dotimes (j n)
                    (when (< (aref arow j) (- +eps+))
                      (let ((rate (/ (- (aref c j) (* (aref cparams j) mu-lo))
                                     (aref arow j))))
                        (when (< rate colmin)
                          (setq col j
                                colmin rate)))))
                  (unless col
                    (error "Pivot not found in row ~A." arow))
                  (dotimes (i m)
                    (setf (aref acol i) (aref a i col)))
                  (incf obj (%pivot row col a b c arow acol dict bparams cparams))))
              ;; primal pivotting
              (let ((col index))
                (dotimes (i m)
                  (setf (aref acol i) (aref a i col)))
                (unless (find-if (lambda (x) (> x +eps+)) acol)
                  (return-from self-dual! (values :dual-infeasible nil nil)))
                (let (row
                      (rowmin +pos-inf+))
                  (dotimes (i m)
                    (when (> (aref acol i) +eps+)
                      (let ((rate (/ (+ (aref b i) (* (aref bparams i) mu-lo))
                                     (aref acol i))))
                        (when (< rate rowmin)
                          (setq row i
                                rowmin rate)))))
                  (unless row
                    (error "Pivot not found in column ~A." acol))
                  (dotimes (j n)
                    (setf (aref arow j) (aref a row j)))
                  (incf obj (%pivot row col a b c arow acol dict bparams cparams)))))))
      (multiple-value-bind (res-primal res-dual) (%restore b c dict)
        (values obj res-primal res-dual)))))
