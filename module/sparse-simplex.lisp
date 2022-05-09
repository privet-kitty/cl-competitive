(defpackage :cp/sparse-simplex
  (:use :cl :cp/csc :cp/lud)
  (:import-from :cp/csc #:+zero+ #:+one+ #:csc-float)
  (:import-from :cp/lud #:vector-set* #:extend-vectorf)
  (:use :cl)
  (:export #:make-dictionary #:sparse-lp #:make-sparse-lp #:slp-restore
           #:slp-lude #:slp-m #:slp-n #:slp-dictionary
           #:slp-mat #:slp-tmat #:slp-b #:slp-c #:slp-x-basic #:slp-y-nonbasic
           #:correct-x-basic! #:correct-y-nonbasic!
           #:dictionary-basis #:dictionary-nonbasis #:dictionary-basic-flag
           #:dictionary-add-basic
           #:slp-primal! #:slp-dual! #:slp-dual-primal! #:slp-self-dual!
           #:*max-number-of-pivotting*
           #:lp-status)
  (:documentation
   "Provides two kinds of simplex method for sparse instance of LP:

- two-phase (dual-then-primal) simplex method using Dantzig's pivot rule;
- parametric self-dual simplex method.

Usage procedure:
1. MAKE-SPARSE-LP
2. SLP-DUAL-PRIMAL!, SLP-SELF-DUAL!, SLP-DUAL!, or SLP-PRIMAL!
3. SLP-RESTORE

Reference:
Robert J. Vanderbei, Linear Programming: Foundations and Extensions, 5th edition."))
(in-package :cp/sparse-simplex)

(defconstant +eps-large+ (coerce 1d-8 'csc-float))
(defconstant +eps-middle+ (coerce 1d-10 'csc-float))
(defconstant +eps-small+ (coerce 1d-12 'csc-float))
(defconstant +inf+ most-positive-double-float)
(defconstant +neg-inf+ most-negative-double-float)

(defun add-slack! (a)
  "Add slack variables to matrix A to transform Ax <= b to [A I][x^t z^t]^t =
b. The dimensions of matrix is changed from m * n to m * (n + m)."
  (declare (optimize (speed 3))
           (csc a))
  (symbol-macrolet ((m (csc-m a))
                    (n (csc-n a))
                    (colstarts (csc-colstarts a))
                    (rows (csc-rows a))
                    (values (csc-values a))
                    (nz (csc-nz a)))
    ;; Add slack variable
    (loop for row below m
          for col of-type (mod #.array-dimension-limit) from n
          for k from (aref colstarts n)
          do (vector-set* values k +one+)
             (vector-set* rows k row)
             (vector-set* colstarts (+ col 1) (+ k 1))
             (incf nz)
          finally (setf n (+ n m)))
    a))

(defstruct (dictionary (:constructor %make-dictionary))
  (m nil :type (mod #.array-dimension-limit))
  (n nil :type (mod #.array-dimension-limit))
  (basis nil :type (simple-array fixnum (*)))
  (nonbasis nil :type (simple-array fixnum (*)))
  (basic-flag nil :type (simple-array fixnum (*))))

(defconstant +nan+ most-positive-fixnum)
(defun make-dictionary (m n basis)
  "BASIS is a vector of column numbers of the constraints matrix which is
currently basic.

0, 1, 2, ....., n-m-1, n-m, n-m+1, ..., n-1
|-- non-slack vars --| |--- slack vars ---|
"
  (declare (optimize (speed 3))
           (vector basis)
           ((mod #.array-dimension-limit) m n))
  (assert (= m (length basis)))
  (assert (<= m n))
  (let* ((basis (if (typep basis '(simple-array fixnum (*)))
                     (copy-seq basis)
                     (coerce basis '(simple-array fixnum (*)))))
         (basic-flag (make-array n :element-type 'fixnum
                                   :initial-element +nan+))
         (nonbasis (make-array (- n m) :element-type 'fixnum)))
    (declare ((simple-array fixnum (*)) basis))
    (dotimes (i m)
      (let ((col (aref basis i)))
        (setf (aref basic-flag col) i)))
    (let ((j 0))
      (dotimes (col (length basic-flag))
        (when (= (aref basic-flag col) +nan+)
          (setf (aref nonbasis j) col
                (aref basic-flag col) (lognot j))
          (incf j))))
    (%make-dictionary :m m :n n :basis basis :nonbasis nonbasis :basic-flag basic-flag)))

(defun dictionary-swap! (dictionary col-out col-in)
  (declare (optimize (speed 3))
           ((mod #.array-dimension-limit) col-out col-in))
  (let* ((basis (dictionary-basis dictionary))
         (nonbasis (dictionary-nonbasis dictionary))
         (basic-flag (dictionary-basic-flag dictionary))
         (i (aref basis col-out))
         (j (aref nonbasis col-in)))
    (setf (aref basis col-out) j
          (aref nonbasis col-in) i
          (aref basic-flag i) (lognot col-in)
          (aref basic-flag j) col-out))
  dictionary)
(defconstant +initial-size+ 16)

(declaim ((simple-array csc-float (*)) *tmp-values*)
         ((simple-array fixnum (*)) *tmp-tags* *tmp-rows*)
         ((integer 0 #.most-positive-fixnum) *tmp-tag*))
(defparameter *tmp-values*
  (make-array +initial-size+ :element-type 'csc-float))
(defparameter *tmp-tags*
  (make-array +initial-size+ :element-type 'fixnum :initial-element 0))
(defparameter *tmp-tag* 1)
(defparameter *tmp-rows* (make-array +initial-size+ :element-type 'fixnum))

(defun tmat-times-vec! (tmat vec basic-flag &optional pivot-index res)
  (declare (optimize (speed 3))
           (csc tmat)
           (sparse-vector vec)
           ((or null sparse-vector) res)
           ((simple-array fixnum (*)) basic-flag)
           ((or null (mod #.array-dimension-limit)) pivot-index))
  (let ((m (csc-m tmat)))
    (extend-vectorf *tmp-values* m)
    (extend-vectorf *tmp-tags* m)
    (extend-vectorf *tmp-rows* m))
  (let ((tmp-values *tmp-values*)
        (tmp-tags *tmp-tags*)
        (tag *tmp-tag*)
        (tmp-rows *tmp-rows*)
        (vector-indices (sparse-vector-indices vec))
        (vector-values (sparse-vector-values vec))
        (tmat-values (csc-values tmat))
        (tmat-colstarts (csc-colstarts tmat))
        (tmat-rows (csc-rows tmat))
        (end 0))
    (dotimes (k1 (sparse-vector-nz vec))
      (let ((col (aref vector-indices k1)))
        (loop for k2 from (aref tmat-colstarts col) below (aref tmat-colstarts (+ col 1))
              for row = (aref tmat-rows k2)
              when (< (aref basic-flag row) 0)
              do (unless (eql tag (aref tmp-tags row))
                   (setf (aref tmp-values row) +zero+
                         (aref tmp-tags row) tag
                         (aref tmp-rows end) row
                         end (+ end 1)))
                 (incf (aref tmp-values row)
                       (* (aref vector-values k1) (aref tmat-values k2))))))
    (let* ((res (or res (make-sparse-vector end)))
           (res-values (sparse-vector-values res))
           (res-indices (sparse-vector-indices res))
           (nz 0))
      (declare ((simple-array csc-float (*)) res-values)
               ((simple-array fixnum (*)) res-indices)
               ((mod #.array-dimension-limit) nz))
      (extend-vectorf res-values end)
      (extend-vectorf res-indices end)
      (dotimes (k end)
        (let* ((row (aref tmp-rows k))
               (index (lognot (aref basic-flag row))))
          ;; In Vanderbei's code the vector components are compared with 1e-8,
          ;; but it appears to be too large to avoid contradiction with the
          ;; decision of an entering column, i.e., in the implementation of
          ;; simplex method below, COL-IN could vanish in this function. I
          ;; artificially avoid that by taking COL-IN as an argument, but maybe
          ;; we should instead choose less EPS here, e.g. 1e-14 as in LU
          ;; factorization in CP/LUD.
          (when (or (eql index pivot-index)
                    (> (abs (aref tmp-values row)) +eps-large+))
            (setf (aref res-values nz) (aref tmp-values row)
                  (aref res-indices nz) index
                  nz (+ nz 1)))))
      (setf (sparse-vector-values res) res-values
            (sparse-vector-indices res) res-indices
            (sparse-vector-nz res) nz)
      (incf *tmp-tag*)
      res)))

(deftype lp-status ()
  "- UNBOUNDED: primal is unbounded;
- INFEASIBLE: primal is infeasible;
- DUAL-INFEASIBLE: dual is infeasible (primal can be either unbounded or infeasible);
- NOT-SOLVED: not solved yet."
  '(member :optimal :unbounded :infeasible :dual-infeasible :not-solved))

(defstruct (sparse-lp (:constructor %make-sparse-lp)
                      (:conc-name slp-))
  (m nil :type (mod #.array-dimension-limit))
  (n nil :type (mod #.array-dimension-limit))
  (mat nil :type csc)
  (tmat nil :type csc)
  (b nil :type (simple-array csc-float (*)))
  (c nil :type (simple-array csc-float (*)))
  (x-basic nil :type (simple-array csc-float (*)))
  (y-nonbasic nil :type (simple-array csc-float (*)))
  (dictionary nil :type dictionary)
  (lude nil :type lud-eta)
  (obj-offset +zero+ :type csc-float))

(defun correct-x-basic! (lude x-basic)
  (assert (zerop (lud-eta-count lude)))
  (dense-solve! (lud-eta-lud lude) x-basic))

(defun correct-y-nonbasic! (sparse-lp)
  (declare (optimize (speed 3)))
  (let* ((lude (slp-lude sparse-lp))
         (m (slp-m sparse-lp))
         (n (slp-n sparse-lp))
         (tmat (slp-tmat sparse-lp))
         (c (slp-c sparse-lp))
         (tmp (make-sparse-vector m))
         (dictionary (slp-dictionary sparse-lp))
         (basis (dictionary-basis dictionary))
         (nonbasis (dictionary-nonbasis dictionary))
         (basic-flag (dictionary-basic-flag dictionary))
         (y-nonbasic (slp-y-nonbasic sparse-lp))
         (tmp-values (sparse-vector-values tmp))
         (tmp-indices (sparse-vector-indices tmp)))
    (symbol-macrolet ((tmp-nz (sparse-vector-nz tmp)))
      (dotimes (i m)
        (let ((coef (aref c (aref basis i))))
          (when (> (abs coef) +eps-large+)
            (setf (aref tmp-values tmp-nz) coef
                  (aref tmp-indices tmp-nz) i)
            (incf tmp-nz))))
      (sparse-solve-transposed! lude tmp)
      (let* ((tmp (tmat-times-vec! tmat tmp basic-flag))
             (tmp-values (sparse-vector-values tmp))
             (tmp-indices (sparse-vector-indices tmp)))
        (dotimes (j (- n m))
          (setf (aref y-nonbasic j) (- (aref c (aref nonbasis j)))))
        (dotimes (k (sparse-vector-nz tmp))
          (incf (aref y-nonbasic (aref tmp-indices k)) (aref tmp-values k)))))
    sparse-lp))

(defun make-sparse-lp (a b c &key (add-slack t) (dictionary nil supplied-p))
  "Creates SPARSE-LP from a sparse matrix, which has the standard form: maximize
c'x subject to Ax <= b, x >= 0.

This function translates a given LP to an equality form Ax + w = b by adding
slack variables and changes A to (A E). If you want to give an equality form
directly, just disable ADD-SLACK.

You can set DICTIONARY to an arbitrary initial dictionary, but please note that
the consequence is undefined when it is rank-deficient.

Note that A is modified when ADD-SLACK is true."
  (declare (optimize (speed 3))
           (csc a)
           ((simple-array csc-float (*)) b c))
  (let* ((m (csc-m a))
         (n (if add-slack (+ (csc-n a) m) (csc-n a))))
    (declare ((integer 0 #.most-positive-fixnum) n))
    (assert (= m (length b)))
    (when add-slack
      (setq a (add-slack! a)))
    ;; Add coefficients for basic variables
    (unless (= (length c) n)
      (assert (= (- n m) (length c)))
      (setq c (adjust-array c n :initial-element +zero+)))
    (let* ((x-basic (make-array m :element-type 'csc-float))
           (y-nonbasic (make-array (- n m) :element-type 'csc-float))
           (dictionary (or dictionary
                           (let ((basis (make-array m :element-type 'fixnum)))
                             (dotimes (i m)
                               (setf (aref basis i) (+ (- n m) i)))
                             (make-dictionary m n basis))))
           (basis (dictionary-basis dictionary))
           (a-transposed (csc-transpose a)))
      (unless supplied-p
        (dotimes (j (- n m))
          (setf (aref y-nonbasic j) (- (aref c j)))))
      (dotimes (i m)
        (setf (aref x-basic i) (aref b i)))
      (let* ((lude (refactor a basis))
             (slp (%make-sparse-lp :m m :n n
                                   :mat a :tmat a-transposed
                                   :b b :c c
                                   :x-basic x-basic
                                   :y-nonbasic y-nonbasic
                                   :dictionary dictionary
                                   :lude lude)))
        (when supplied-p
          (correct-x-basic! lude x-basic)
          (correct-y-nonbasic! slp))
        slp))))

(declaim (ftype (function * (values csc-float &optional)) dot*))
(defun dot* (coefs x-basic basis)
  (declare (optimize (speed 3))
           ((simple-array csc-float (*)) coefs x-basic)
           ((simple-array fixnum (*)) basis))
  (let ((res +zero+))
    (declare (csc-float res))
    (dotimes (i (length x-basic))
      (incf res (* (aref coefs (aref basis i)) (aref x-basic i))))
    res))

(defun slp-restore (sparse-lp)
  "Restores the current solution of LP and returns five values: objective value,
primal solution, dual solution. (Note that they are not necessarily feasible
solutions if the current dictionary is not feasible.)

Structure of primal solution:
0, 1, 2, ......, n-m+1, n-m, n-m+1, ....., n-1
|-- primal solution --| |--- slack values ---|

Structure of dual solution:
0, 1, 2, ......, n-m+1, n-m, n-m+1, ....., n-1
|--- slack values ---| |--- dual solution ---|
"
  (declare (optimize (speed 3)))
  (let* ((m (slp-m sparse-lp))
         (n (slp-n sparse-lp))
         (c (slp-c sparse-lp))
         (x-basic (slp-x-basic sparse-lp))
         (y-nonbasic (slp-y-nonbasic sparse-lp))
         (dictionary (slp-dictionary sparse-lp))
         (basis (dictionary-basis dictionary))
         (nonbasis (dictionary-nonbasis dictionary))
         (x (make-array n :element-type 'csc-float :initial-element +zero+))
         (y (make-array n :element-type 'csc-float :initial-element +zero+)))
    (dotimes (i m)
      (setf (aref x (aref basis i)) (aref x-basic i)))
    (dotimes (i (- n m))
      (setf (aref y (aref nonbasis i)) (aref y-nonbasic i)))
    (values (+ (slp-obj-offset sparse-lp) (dot* c x-basic basis))
            x y)))

(defun pick-negative (vector)
  (declare (optimize (speed 3))
           ((simple-array csc-float (*)) vector))
  (let ((min (- +eps-small+))
        res)
    (dotimes (i (length vector))
      (when (< (aref vector i) min)
        (setq min (aref vector i)
              res i)))
    res))

(defun ratio-test (x dx)
  (declare (optimize (speed 3))
           ((simple-array csc-float (*)) x)
           (sparse-vector dx))
  (let ((min +inf+)
        (dx-indices (sparse-vector-indices dx))
        (dx-values (sparse-vector-values dx))
        res)
    (dotimes (k (sparse-vector-nz dx))
      (when (> (aref dx-values k) +eps-large+)
        (let* ((index (aref dx-indices k))
               (rate (/ (aref x index) (aref dx-values k))))
          (when (< rate min)
            (setq min rate
                  res index)))))
    res))

(declaim ((integer 0 #.most-positive-fixnum) *max-number-of-pivotting*))
(defparameter *max-number-of-pivotting* most-positive-fixnum)

(declaim (ftype (function * (values lp-status (integer 0 #.most-positive-fixnum) &optional))
                slp-primal! slp-dual! slp-dual-primal! slp-self-dual!))

(defun slp-primal! (sparse-lp)
  "Applies primal simplex method to SPARSE-LP, and returns the terminal state
and the number of pivotting: Note that this function doesn't check if the
initial dictionary is primal feasible."
  (declare (optimize (speed 3)))
  (let* ((m (slp-m sparse-lp))
         (n (slp-n sparse-lp))
         (x-basic (slp-x-basic sparse-lp))
         (y-nonbasic (slp-y-nonbasic sparse-lp))
         (dictionary (slp-dictionary sparse-lp))
         (basis (dictionary-basis dictionary))
         (nonbasis (dictionary-nonbasis dictionary))
         (basic-flag (dictionary-basic-flag dictionary))
         (mat (slp-mat sparse-lp))
         (tmat (slp-tmat sparse-lp))
         (dx (make-sparse-vector m))
         (dy (make-sparse-vector (- n m)))
         (tmp (make-sparse-vector m)))
    (symbol-macrolet ((lude (slp-lude sparse-lp))
                      (dx-values (sparse-vector-values dx))
                      (dx-indices (sparse-vector-indices dx))
                      (dx-nz (sparse-vector-nz dx))
                      (dy-values (sparse-vector-values dy))
                      (dy-indices (sparse-vector-indices dy))
                      (dy-nz (sparse-vector-nz dy))
                      (tmp-values (sparse-vector-values tmp))
                      (tmp-indices (sparse-vector-indices tmp))
                      (tmp-nz (sparse-vector-nz tmp)))
      (dotimes (n-pivot *max-number-of-pivotting* (values :not-solved n-pivot))
        ;; find entering column
        (let* ((col-in (pick-negative y-nonbasic)))
          (unless col-in
            (return (values :optimal n-pivot)))
          ;; dx_B := B^(-1)Ne_j (j = col-in)
          (let ((acolstarts (csc-colstarts mat))
                (arows (csc-rows mat))
                (avalues (csc-values mat))
                (j (aref nonbasis col-in)))
            (loop for i from 0
                  for k from (aref acolstarts j) below (aref acolstarts (+ j 1))
                  do (setf (aref dx-values i) (aref avalues k)
                           (aref dx-indices i) (aref arows k))
                  finally (setq dx-nz i)))
          (sparse-solve! lude dx)
          ;; find leaving column
          (let ((col-out (ratio-test x-basic dx)))
            (unless col-out
              (return (values :unbounded n-pivot)))
            ;; dy_N := -(B^(-1)N)^Te_i (i = col-out)
            (setf (aref tmp-values 0) (- +one+)
                  (aref tmp-indices 0) col-out
                  tmp-nz 1)
            (sparse-solve-transposed! lude tmp)
            (tmat-times-vec! tmat tmp basic-flag col-in dy)
            ;; t := x_i/dx_i
            ;; s := y_j/dy_j
            (let ((rate-t (loop for k below dx-nz
                                when (= (aref dx-indices k) col-out)
                                do (return (/ (aref x-basic col-out)
                                              (aref dx-values k)))
                                finally (error "Huh?")))
                  (rate-s (loop for k below dy-nz
                                when (= (aref dy-indices k) col-in)
                                do (return (/ (aref y-nonbasic col-in)
                                              (aref dy-values k)))
                                finally (error "Huh?"))))
              ;; y_N := y_N - s dy_N
              ;; y_i := s
              ;; x_B := x_B - t dx_B
              ;; x_j := t
              (dotimes (k dy-nz)
                (let ((j (aref dy-indices k)))
                  (decf (aref y-nonbasic j) (* rate-s (aref dy-values k)))))
              (setf (aref y-nonbasic col-in) rate-s)
              (dotimes (k dx-nz)
                (let ((i (aref dx-indices k)))
                  (decf (aref x-basic i) (* rate-t (aref dx-values k)))))
              (setf (aref x-basic col-out) rate-t)
              ;; Update basis
              (dictionary-swap! dictionary col-out col-in)
              (add-eta! lude col-out dx)
              (when (refactor-p lude col-out)
                (setq lude (refactor mat basis))))))))))

(defun slp-dual! (sparse-lp)
  "Applies dual simplex method to SPARSE-LP, and returns the terminal state and
the number of pivotting. Note that this function doesn't check if the initial
dictionary is dual feasible."
  (declare (optimize (speed 3)))
  (let* ((m (slp-m sparse-lp))
         (n (slp-n sparse-lp))
         (x-basic (slp-x-basic sparse-lp))
         (y-nonbasic (slp-y-nonbasic sparse-lp))
         (dictionary (slp-dictionary sparse-lp))
         (basis (dictionary-basis dictionary))
         (nonbasis (dictionary-nonbasis dictionary))
         (basic-flag (dictionary-basic-flag dictionary))
         (mat (slp-mat sparse-lp))
         (tmat (slp-tmat sparse-lp))
         (dx (make-sparse-vector m))
         (dy (make-sparse-vector (- n m)))
         (tmp (make-sparse-vector m)))
    (symbol-macrolet ((lude (slp-lude sparse-lp))
                      (dx-values (sparse-vector-values dx))
                      (dx-indices (sparse-vector-indices dx))
                      (dx-nz (sparse-vector-nz dx))
                      (dy-values (sparse-vector-values dy))
                      (dy-indices (sparse-vector-indices dy))
                      (dy-nz (sparse-vector-nz dy))
                      (tmp-values (sparse-vector-values tmp))
                      (tmp-indices (sparse-vector-indices tmp))
                      (tmp-nz (sparse-vector-nz tmp)))
      (dotimes (n-pivot *max-number-of-pivotting* (values :not-solved n-pivot))
        ;; find leaving column
        (let ((col-out (pick-negative x-basic)))
          (unless col-out
            (return (values :optimal n-pivot)))
          ;; dy_N := -(B^(-1)N)^Te_i (i = col-out)
          (setf (aref tmp-values 0) (- +one+)
                (aref tmp-indices 0) col-out
                tmp-nz 1)
          (sparse-solve-transposed! lude tmp)
          (tmat-times-vec! tmat tmp basic-flag nil dy)
          ;; find entering column
          (let ((col-in (ratio-test y-nonbasic dy)))
            (unless col-in
              (return (values :infeasible n-pivot)))
            ;; dx_B := B^(-1)Ne_j (j = col-in)
            (let ((acolstarts (csc-colstarts mat))
                  (arows (csc-rows mat))
                  (avalues (csc-values mat))
                  (j (aref nonbasis col-in)))
              (loop for i from 0
                    for k from (aref acolstarts j) below (aref acolstarts (+ j 1))
                    do (setf (aref dx-values i) (aref avalues k)
                             (aref dx-indices i) (aref arows k))
                    finally (setq dx-nz i)))
            (sparse-solve! lude dx)
            ;; t := x_i/dx_i
            ;; s := y_j/dy_j
            (let ((rate-t (loop for k below dx-nz
                                when (= (aref dx-indices k) col-out)
                                do (return (/ (aref x-basic col-out)
                                              (aref dx-values k)))
                                finally (error "Huh?")))
                  (rate-s (loop for k below dy-nz
                                when (= (aref dy-indices k) col-in)
                                do (return (/ (aref y-nonbasic col-in)
                                              (aref dy-values k)))
                                finally (error "Huh?"))))
              ;; y_N := y_N - s dy_N
              ;; y_i := s
              ;; x_B := x_B - t dx_B
              ;; x_j := t
              (dotimes (k dy-nz)
                (let ((j (aref dy-indices k)))
                  (decf (aref y-nonbasic j) (* rate-s (aref dy-values k)))))
              (setf (aref y-nonbasic col-in) rate-s)
              (dotimes (k dx-nz)
                (let ((i (aref dx-indices k)))
                  (decf (aref x-basic i) (* rate-t (aref dx-values k)))))
              (setf (aref x-basic col-out) rate-t)
              ;; Update basis
              (dictionary-swap! dictionary col-out col-in)
              (add-eta! lude col-out dx)
              (when (refactor-p lude col-out)
                (setq lude (refactor mat basis))))))))))

(defun slp-dual-primal! (sparse-lp)
  "Applies two-phase simplex method to SPARSE-LP and returns the terminal state:
:optimal, :unbounded, or :infeasible. "
  (declare (optimize (speed 3)))
  (let* ((m (slp-m sparse-lp))
         (n (slp-n sparse-lp))
         (dictionary (slp-dictionary sparse-lp))
         (nonbasis (dictionary-nonbasis dictionary))
         (y-nonbasic (slp-y-nonbasic sparse-lp))
         (c (slp-c sparse-lp)))
    ;; Set all the coefficients of the objective to negative values.
    (dotimes (j (- n m))
      (let ((col (aref nonbasis j)))
        (setf (aref y-nonbasic j)
              (+ (max (aref c col) +one+)
                 (random +one+)))))
    (multiple-value-bind (status1 n-pivot1) (slp-dual! sparse-lp)
      (correct-y-nonbasic! sparse-lp)
      (unless (eql status1 :optimal)
        (return-from slp-dual-primal! (values status1 n-pivot1)))
      (multiple-value-bind (status2 n-pivot2) (slp-primal! sparse-lp)
        (values status2 (+ n-pivot1 n-pivot2))))))

;;;
;;; self-dual simplex method
;;;

(defun self-dual-ratio-test (x dx mu x-params)
  (declare (optimize (speed 3))
           ((simple-array csc-float (*)) x x-params)
           (sparse-vector dx)
           (csc-float mu))
  (let ((min +inf+)
        (dx-indices (sparse-vector-indices dx))
        (dx-values (sparse-vector-values dx))
        res)
    (dotimes (k (sparse-vector-nz dx))
      (when (> (aref dx-values k) +eps-large+)
        (let* ((index (aref dx-indices k))
               (rate (/ (+ (aref x index) (* mu (aref x-params index)))
                        (aref dx-values k))))
          (when (< rate min)
            (setq min rate
                  res index)))))
    res))

(defun slp-self-dual! (sparse-lp)
  "Applies self-dual simplex method to SPARSE-LP, and returns the terminal state and the number of pivotting.

Note that this function could return either :infeasible or :dual-infeasible for
a both infeasible instance. (It is not even deterministic.)"
  (declare (optimize (speed 3)))
  (let* ((m (slp-m sparse-lp))
         (n (slp-n sparse-lp))
         (x-basic (slp-x-basic sparse-lp))
         (y-nonbasic (slp-y-nonbasic sparse-lp))
         (dictionary (slp-dictionary sparse-lp))
         (basis (dictionary-basis dictionary))
         (nonbasis (dictionary-nonbasis dictionary))
         (basic-flag (dictionary-basic-flag dictionary))
         (mat (slp-mat sparse-lp))
         (tmat (slp-tmat sparse-lp))
         (dx (make-sparse-vector m))
         (dy (make-sparse-vector (- n m)))
         (tmp (make-sparse-vector m))
         (x-params (make-array m :element-type 'csc-float :initial-element +zero+))
         (y-params (make-array (- n m) :element-type 'csc-float :initial-element +zero+)))
    (symbol-macrolet ((lude (slp-lude sparse-lp))
                      (dx-values (sparse-vector-values dx))
                      (dx-indices (sparse-vector-indices dx))
                      (dx-nz (sparse-vector-nz dx))
                      (dy-values (sparse-vector-values dy))
                      (dy-indices (sparse-vector-indices dy))
                      (dy-nz (sparse-vector-nz dy))
                      (tmp-values (sparse-vector-values tmp))
                      (tmp-indices (sparse-vector-indices tmp))
                      (tmp-nz (sparse-vector-nz tmp)))
      ;; initialize parameters for x and y
      (dotimes (j n)
        (let ((colstarts (csc-colstarts mat))
              (rows (csc-rows mat))
              (values (csc-values mat)))
          (loop for k from (aref colstarts j) below (aref colstarts (+ j 1))
                for a2 of-type csc-float = (expt (aref values k) 2)
                do (incf (aref x-params (aref rows k)) a2)
                   (when (< (aref basic-flag j) 0)
                     (incf (aref y-params (lognot (aref basic-flag j))) a2)))))
      (map-into x-params
                (lambda (x) (+ (random +one+) (sqrt (the (double-float #.+zero+) x))))
                x-params)
      (map-into y-params
                (lambda (x) (+ (random +one+) (sqrt (the (double-float #.+zero+) x))))
                y-params)
      (dotimes (n-pivot *max-number-of-pivotting* (values :not-solved n-pivot))
        (let ((mu +neg-inf+)
              col-in
              col-out)
          (dotimes (j (- n m))
            (when (and (> (aref y-params j) +eps-small+)
                       (< mu (/ (- (aref y-nonbasic j)) (aref y-params j))))
              (setq mu (/ (- (aref y-nonbasic j)) (aref y-params j))
                    col-in j)))
          (dotimes (i m)
            (when (and (> (aref x-params i) +eps-small+)
                       (< mu (/ (- (aref x-basic i)) (aref x-params i))))
              (setq mu (/ (- (aref x-basic i)) (aref x-params i))
                    col-out i
                    col-in nil)))
          (when (<= mu +eps-middle+)
            (return (values :optimal n-pivot)))
          (assert (or (and col-in (not col-out))
                      (and (not col-in) col-out)))
          (if col-out
              (progn
                ;; dy_N := -(B^(-1)N)^T e_i where i is leaving column
                (setf (aref tmp-values 0) (- +one+)
                      (aref tmp-indices 0) col-out
                      tmp-nz 1)
                (sparse-solve-transposed! lude tmp)
                (tmat-times-vec! tmat tmp basic-flag col-in dy)
                (setq col-in (self-dual-ratio-test y-nonbasic dy mu y-params))
                (unless col-in
                  (return (values :infeasible n-pivot)))
                ;; dx_B := B^(-1)Ne_j where j is entering column
                (let* ((j (aref nonbasis col-in))
                       (colstarts (csc-colstarts mat))
                       (rows (csc-rows mat))
                       (values (csc-values mat))
                       (start (aref colstarts j))
                       (end (aref colstarts (+ j 1))))
                  (loop for k from start below end
                        for i of-type (mod #.array-dimension-limit) = (- k start)
                        do (setf (aref dx-values i) (aref values k)
                                 (aref dx-indices i) (aref rows k)))
                  (setq dx-nz (- end start))
                  (sparse-solve! lude dx)))
              ;; dx_B := B^(-1)Ne_j where j is entering column
              (let* ((j (aref nonbasis col-in))
                     (colstarts (csc-colstarts mat))
                     (rows (csc-rows mat))
                     (values (csc-values mat))
                     (start (aref colstarts j))
                     (end (aref colstarts (+ j 1))))
                (loop for k from start below end
                      for i of-type (mod #.array-dimension-limit) = (- k start)
                      do (setf (aref dx-values i) (aref values k)
                               (aref dx-indices i) (aref rows k)))
                (setq dx-nz (- end start))
                (sparse-solve! lude dx)
                (setq col-out (self-dual-ratio-test x-basic dx mu x-params))
                (unless col-out
                  (return (values :dual-infeasible n-pivot)))
                ;; dy_N := -(B^(-1)N)^T e_i where i is leaving column
                (setf (aref tmp-values 0) (- +one+)
                      (aref tmp-indices 0) col-out
                      tmp-nz 1)
                (sparse-solve-transposed! lude tmp)
                (tmat-times-vec! tmat tmp basic-flag col-in dy)))
          ;; t := x_i/dx_i
          ;; tparam := xparam_i/dx_i
          ;; s := y_j/dy_j
          ;; sparam := y_param_j/dy_j
          (multiple-value-bind (rate-t rate-tparam)
              (dotimes (k dx-nz (error "Huh?"))
                (when (eql (aref dx-indices k) col-out)
                  (return (values (/ (aref x-basic col-out)
                                     (aref dx-values k))
                                  (/ (aref x-params col-out)
                                     (aref dx-values k))))))
            (multiple-value-bind (rate-s rate-sparam)
                (dotimes (k dy-nz (error "Huh?"))
                  (when (eql (aref dy-indices k) col-in)
                    (return (values (/ (aref y-nonbasic col-in)
                                       (aref dy-values k))
                                    (/ (aref y-params col-in)
                                       (aref dy-values k))))))
              ;; y_N := y_N - s dy_N
              ;; yparam := yparam - s dy_N
              ;; y_i := s
              ;; x_B := x_B - t dx_B
              ;; xparam_B := xparam - t dx_B
              ;; x_j := t
              (dotimes (k dy-nz)
                (let ((j (aref dy-indices k)))
                  (decf (aref y-nonbasic j) (* rate-s (aref dy-values k)))
                  (decf (aref y-params j) (* rate-sparam (aref dy-values k)))))
              (setf (aref y-nonbasic col-in) rate-s
                    (aref y-params col-in) rate-sparam)
              (dotimes (k dx-nz)
                (let ((i (aref dx-indices k)))
                  (decf (aref x-basic i) (* rate-t (aref dx-values k)))
                  (decf (aref x-params i) (* rate-tparam (aref dx-values k)))))
              (setf (aref x-basic col-out) rate-t
                    (aref x-params col-out) rate-tparam)
              ;; Update basis
              (dictionary-swap! dictionary col-out col-in)
              (add-eta! lude col-out dx)
              (when (refactor-p lude col-out)
                (setq lude (refactor mat basis))))))))))
