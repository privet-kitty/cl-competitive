(defpackage :cp/sparse-simplex
  (:use :cl :cp/csc :cp/lud)
  (:import-from :cp/csc #:+zero+ #:+one+ #:csc-float)
  (:import-from :cp/lud #:vector-set* #:extend-vectorf)
  (:use :cl)
  (:export #:make-dictionary #:make-sparse-lp #:slp-restore
           #:slp-lude #:slp-m #:slp-n #:slp-dictionary
           #:slp-mat #:slp-tmat #:slp-b #:slp-c #:slp-x-basic #:slp-y-nonbasic
           #:correct-x-basic! #:correct-y-nonbasic!
           #:dictionary-basics #:dictionary-nonbasics #:dictionary-basic-flag
           #:dictionary-add-basic
           #:slp-primal! #:slp-dual! #:slp-dual-primal! #:slp-self-dual!)
  (:documentation
   "Provides two kinds of simplex method for sparse LP:

- two-phase (dual-then-primal) simplex method using Dantzig's pivot rule;
- parametric self-dual simplex method.

Usage procedure:
1. MAKE-SPARSE-LP
2. SLP-DUAL-PRIMAL!, SLP-SELF-DUAL!, SLP-DUAL!, or SLP-PRIMAL!
3. SRARSE-LP-RESTORE

Reference:
Robert J. Vanderbei. Linear Programming: Foundations and Extensions. 5th edition."))
(in-package :cp/sparse-simplex)

(defconstant +eps-large+ (coerce 1d-8 'csc-float))
(defconstant +eps-middle+ (coerce 1d-10 'csc-float))
(defconstant +eps-small+ (coerce 1d-12 'csc-float))
(defconstant +inf+ most-positive-double-float)
(defconstant +neg-inf+ most-negative-double-float)

(defun add-slack! (a)
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
  (basics nil :type (simple-array fixnum (*)))
  (nonbasics nil :type (simple-array fixnum (*)))
  (basic-flag nil :type (simple-array fixnum (*))))

(defconstant +nan+ most-positive-fixnum)
(defun make-dictionary (m n basics)
  "BASICS[i] = column number of the constraints matrix (of size m * (n + m))
which is currently basic.

0, 1, 2, ....., n-1, n, n+1, ..., n+m-1
|- non-slack vars -| |-- slack vars --|
"
  (declare (optimize (speed 3))
           (vector basics)
           ((mod #.array-dimension-limit) m n))
  (assert (= m (length basics)))
  (let* ((basics (if (typep basics '(simple-array fixnum (*)))
                     (copy-seq basics)
                     (coerce basics '(simple-array fixnum (*)))))
         (basic-flag (make-array (+ n m) :element-type 'fixnum :initial-element +nan+))
         (nonbasics (make-array n :element-type 'fixnum)))
    (declare ((simple-array fixnum (*)) basics))
    (dotimes (i m)
      (let ((col (aref basics i)))
        (setf (aref basic-flag col) i)))
    (let ((j 0))
      (dotimes (col (length basic-flag))
        (when (= (aref basic-flag col) +nan+)
          (setf (aref nonbasics j) col
                (aref basic-flag col) (lognot j))
          (incf j))))
    (%make-dictionary :m m :n n :basics basics :nonbasics nonbasics :basic-flag basic-flag)))

(defun dictionary-add-basic! (dictionary)
  (symbol-macrolet ((basics (dictionary-basics dictionary))
                    (basic-flag (dictionary-basic-flag dictionary))
                    (m (dictionary-m dictionary))
                    (n (dictionary-n dictionary)))
    (when (< (length basics) (+ m 1))
      (setq basics (adjust-array basics (* 2 (+ m 1)))))
    (when (< (length basic-flag) (+ n m 1))
      (setq basic-flag (adjust-array basic-flag (* 2 (+ n m 1)))))
    (let ((new-col (+ m n)))
      (declare ((mod #.array-dimension-limit) new-col))
      (setf (aref basics m) new-col
            (aref basic-flag new-col) m)
      (incf m)
      new-col)))

(defun dictionary-swap! (dictionary col-out col-in)
  (declare (optimize (speed 3))
           ((mod #.array-dimension-limit) col-out col-in))
  (let* ((basics (dictionary-basics dictionary))
         (nonbasics (dictionary-nonbasics dictionary))
         (basic-flag (dictionary-basic-flag dictionary))
         (i (aref basics col-out))
         (j (aref nonbasics col-in)))
    (setf (aref basics col-out) j
          (aref nonbasics col-in) i
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

(defun tmat-times-vec! (tmat vec basic-flag &optional res)
  (declare (optimize (speed 3))
           (csc tmat)
           (sparse-vector vec)
           ((or null sparse-vector) res)
           ((simple-array fixnum (*)) basic-flag))
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
    (declare ((mod #.array-dimension-limit)))
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
        (let ((row (aref tmp-rows k)))
          (when (> (abs (aref tmp-values row)) +eps-large+)
            (setf (aref res-values nz) (aref tmp-values row)
                  (aref res-indices nz) (lognot (aref basic-flag row))
                  nz (+ nz 1)))))
      (setf (sparse-vector-values res) res-values
            (sparse-vector-indices res) res-indices
            (sparse-vector-nz res) nz)
      (incf *tmp-tag*)
      res)))

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
  (lude nil :type lud-eta))

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
         (basics (dictionary-basics dictionary))
         (nonbasics (dictionary-nonbasics dictionary))
         (basic-flag (dictionary-basic-flag dictionary))
         (y-nonbasic (slp-y-nonbasic sparse-lp))
         (tmp-values (sparse-vector-values tmp))
         (tmp-indices (sparse-vector-indices tmp)))
    (symbol-macrolet ((tmp-nz (sparse-vector-nz tmp)))
      (dotimes (i m)
        (let ((coef (aref c (aref basics i))))
          (when (> (abs coef) +eps-large+)
            (setf (aref tmp-values tmp-nz) coef
                  (aref tmp-indices tmp-nz) i)
            (incf tmp-nz))))
      (sparse-solve-transposed! lude tmp)
      (let* ((tmp (tmat-times-vec! tmat tmp basic-flag))
             (tmp-values (sparse-vector-values tmp))
             (tmp-indices (sparse-vector-indices tmp)))
        (dotimes (j n)
          (setf (aref y-nonbasic j) (- (aref c (aref nonbasics j)))))
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
         (n (if add-slack (csc-n a) (- (csc-n a) (csc-m a)))))
    (assert (= m (length b)))
    (when add-slack
      (setq a (add-slack! a)))
    ;; Add coefficients for basic variables
    (unless (= (length c) (+ m n))
      (assert (= n (length c)))
      (setq c (adjust-array c (the (mod #.array-dimension-limit) (+ n m))
                            :initial-element +zero+)))
    (let* ((x-basic (make-array m :element-type 'csc-float))
           (y-nonbasic (make-array n :element-type 'csc-float))
           (dictionary (or dictionary
                           (let ((basics (make-array m :element-type 'fixnum)))
                             (dotimes (i m)
                               (setf (aref basics i) (+ n i)))
                             (make-dictionary m n basics))))
           (basics (dictionary-basics dictionary))
           (a-transposed (csc-transpose a)))
      (unless supplied-p
        (dotimes (j n)
          (setf (aref y-nonbasic j) (- (aref c j)))))
      (dotimes (i m)
        (setf (aref x-basic i) (aref b i)))
      (let* ((lude (refactor a basics))
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
(defun dot* (coefs x-basic basics)
  (declare (optimize (speed 3))
           ((simple-array csc-float (*)) coefs x-basic)
           ((simple-array fixnum (*)) basics))
  (let ((res +zero+))
    (declare (csc-float res))
    (dotimes (i (length x-basic))
      (incf res (* (aref coefs (aref basics i)) (aref x-basic i))))
    res))

(defun slp-restore (sparse-lp)
  "Restores the current solution of LP and returns five values: objective value,
primal solution, dual solution, values of primal slack variables, and values of
dual slack variables. (Note that they are not necessarily feasible solutions if
the current dictionary is not feasible.)"
  (declare (optimize (speed 3)))
  (let* ((m (slp-m sparse-lp))
         (n (slp-n sparse-lp))
         (c (slp-c sparse-lp))
         (x-basic (slp-x-basic sparse-lp))
         (y-nonbasic (slp-y-nonbasic sparse-lp))
         (dictionary (slp-dictionary sparse-lp))
         (basics (dictionary-basics dictionary))
         (nonbasics (dictionary-nonbasics dictionary))
         (x (make-array (+ n m) :element-type 'csc-float :initial-element +zero+))
         (y (make-array (+ n m) :element-type 'csc-float :initial-element +zero+)))
    (dotimes (i m)
      (setf (aref x (aref basics i)) (aref x-basic i)))
    (dotimes (i n)
      (setf (aref y (aref nonbasics i)) (aref y-nonbasic i)))
    (values (dot* c x-basic basics)
            (subseq x 0 n)
            (subseq y n)
            (subseq x n)
            (subseq y 0 n))))

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

(defun slp-primal! (sparse-lp)
  "Applies primal simplex method to SPARSE-LP and returns the terminal state:
:optimal or :unbounded. Note that this function doesn't check if the initial
dictionary is primal feasible."
  (declare (optimize (speed 3)))
  (let* ((m (slp-m sparse-lp))
         (n (slp-n sparse-lp))
         (x-basic (slp-x-basic sparse-lp))
         (y-nonbasic (slp-y-nonbasic sparse-lp))
         (dictionary (slp-dictionary sparse-lp))
         (basics (dictionary-basics dictionary))
         (nonbasics (dictionary-nonbasics dictionary))
         (basic-flag (dictionary-basic-flag dictionary))
         (mat (slp-mat sparse-lp))
         (tmat (slp-tmat sparse-lp))
         (dx (make-sparse-vector m))
         (dy (make-sparse-vector n))
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
      (loop
        ;; find entering column
        (let* ((col-in (pick-negative y-nonbasic)))
          (unless col-in
            (return :optimal))
          ;; dx_B := B^(-1)Ne_j (j = col-in)
          (let ((acolstarts (csc-colstarts mat))
                (arows (csc-rows mat))
                (avalues (csc-values mat))
                (j (aref nonbasics col-in)))
            (loop for i from 0
                  for k from (aref acolstarts j) below (aref acolstarts (+ j 1))
                  do (setf (aref dx-values i) (aref avalues k)
                           (aref dx-indices i) (aref arows k))
                  finally (setq dx-nz i)))
          (sparse-solve! lude dx)
          ;; find leaving column
          (let ((col-out (ratio-test x-basic dx)))
            (unless col-out
              (return :unbounded))
            ;; dy_N := -(B^(-1)N)^Te_i (i = col-out)
            (setf (aref tmp-values 0) (- +one+)
                  (aref tmp-indices 0) col-out
                  tmp-nz 1)
            (sparse-solve-transposed! lude tmp)
            (tmat-times-vec! tmat tmp basic-flag dy)
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
                (setq lude (refactor mat basics))))))))))

(defun slp-dual! (sparse-lp)
  "Applies dual simplex method to SPARSE-LP and returns the terminal state:
:optimal or :infeasible. Note that this function doesn't check if the initial
dictionary is dual feasible."
  (declare (optimize (speed 3)))
  (let* ((m (slp-m sparse-lp))
         (n (slp-n sparse-lp))
         (x-basic (slp-x-basic sparse-lp))
         (y-nonbasic (slp-y-nonbasic sparse-lp))
         (dictionary (slp-dictionary sparse-lp))
         (basics (dictionary-basics dictionary))
         (nonbasics (dictionary-nonbasics dictionary))
         (basic-flag (dictionary-basic-flag dictionary))
         (mat (slp-mat sparse-lp))
         (tmat (slp-tmat sparse-lp))
         (dx (make-sparse-vector m))
         (dy (make-sparse-vector n))
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
      (loop
        ;; find leaving column
        (let ((col-out (pick-negative x-basic)))
          (unless col-out
            (return :optimal))
          ;; dy_N := -(B^(-1)N)^Te_i (i = col-out)
          (setf (aref tmp-values 0) (- +one+)
                (aref tmp-indices 0) col-out
                tmp-nz 1)
          (sparse-solve-transposed! lude tmp)
          (tmat-times-vec! tmat tmp basic-flag dy)
          ;; find entering column
          (let ((col-in (ratio-test y-nonbasic dy)))
            (unless col-in
              (return :infeasible))
            ;; dx_B := B^(-1)Ne_j (j = col-in)
            (let ((acolstarts (csc-colstarts mat))
                  (arows (csc-rows mat))
                  (avalues (csc-values mat))
                  (j (aref nonbasics col-in)))
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
                (setq lude (refactor mat basics))))))))))

(defun slp-dual-primal! (sparse-lp)
  "Applies two-phase simplex method to SPARSE-LP and returns the terminal state:
:optimal, :unbounded, or :infeasible. "
  (declare (optimize (speed 3)))
  (let* ((n (slp-n sparse-lp))
         (dictionary (slp-dictionary sparse-lp))
         (nonbasics (dictionary-nonbasics dictionary))
         (y-nonbasic (slp-y-nonbasic sparse-lp))
         (c (slp-c sparse-lp)))
    ;; Set all the coefficiets of objective to negative values.
    (dotimes (j n)
      (let ((col (aref nonbasics j)))
        (setf (aref y-nonbasic j)
              (+ (max (if (< col n) (aref c col) +zero+) +one+)
                 (random +one+)))))
    (let ((state-dual (slp-dual! sparse-lp)))
      (correct-y-nonbasic! sparse-lp)
      (unless (eql state-dual :optimal)
        (return-from slp-dual-primal! state-dual))
      (slp-primal! sparse-lp))))

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
  "Applies self-dual simplex method to SPARSE-LP and returns the terminal state:
:optimal, :infeasible, or :dual-infeasible.

Note that this function could return either :infeasible or :dual-infeasible for
a both infeasible instance. (It is not even deterministic.)"
  (declare (optimize (speed 3)))
  (let* ((m (slp-m sparse-lp))
         (n (slp-n sparse-lp))
         (x-basic (slp-x-basic sparse-lp))
         (y-nonbasic (slp-y-nonbasic sparse-lp))
         (dictionary (slp-dictionary sparse-lp))
         (basics (dictionary-basics dictionary))
         (nonbasics (dictionary-nonbasics dictionary))
         (basic-flag (dictionary-basic-flag dictionary))
         (mat (slp-mat sparse-lp))
         (tmat (slp-tmat sparse-lp))
         (dx (make-sparse-vector m))
         (dy (make-sparse-vector n))
         (tmp (make-sparse-vector m))
         (x-params (make-array m :element-type 'csc-float :initial-element +zero+))
         (y-params (make-array n :element-type 'csc-float :initial-element +zero+)))
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
      ;; FIXME: make it for arbitrary initial dictionary
      ;; initialize parameters for x and y
      (dotimes (j n)
        (let ((colstarts (csc-colstarts mat))
              (rows (csc-rows mat))
              (values (csc-values mat)))
          (loop for k from (aref colstarts j) below (aref colstarts (+ j 1))
                for a2 of-type csc-float = (expt (aref values k) 2)
                do (incf (aref x-params (aref rows k)) a2)
                   (incf (aref y-params j) a2))))
      (map-into x-params
                (lambda (x) (+ (random +one+) (sqrt (the (double-float #.+zero+) x))))
                x-params)
      (map-into y-params
                (lambda (x) (+ (random +one+) (sqrt (the (double-float #.+zero+) x))))
                y-params)
      (loop
        (let ((mu +neg-inf+)
              col-in
              col-out)
          (dotimes (j n)
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
            (return :optimal))
          (assert (or (and col-in (not col-out))
                      (and (not col-in) col-out)))
          (if col-out
              (progn
                ;; dy_N := -(B^(-1)N)^T e_i where i is leaving column
                (setf (aref tmp-values 0) (- +one+)
                      (aref tmp-indices 0) col-out
                      tmp-nz 1)
                (sparse-solve-transposed! lude tmp)
                (tmat-times-vec! tmat tmp basic-flag dy)
                (setq col-in (self-dual-ratio-test y-nonbasic dy mu y-params))
                (unless col-in
                  (return :infeasible))
                ;; dx_B := B^(-1)Ne_j where j is entering column
                (let* ((j (aref nonbasics col-in))
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
              (let* ((j (aref nonbasics col-in))
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
                  (return :dual-infeasible))
                ;; dy_N := -(B^(-1)N)^T e_i where i is leaving column
                (setf (aref tmp-values 0) (- +one+)
                      (aref tmp-indices 0) col-out
                      tmp-nz 1)
                (sparse-solve-transposed! lude tmp)
                (tmat-times-vec! tmat tmp basic-flag dy)))
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
                (setq lude (refactor mat basics))))))))))
