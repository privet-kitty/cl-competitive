(defpackage :cp/sparse-two-phase-simplex
  (:use :cl :cp/csc :cp/lud :cp/sparse-simplex-common)
  (:import-from :cp/csc #:+zero+ #:+one+ #:csc-float)
  (:import-from :cp/lud #:vector-set* #:extend-vectorf)
  (:export #:slp-primal! #:slp-dual! #:slp-dual-primal! )
  (:documentation "Provides two-phase (dual-then-primal) simplex method for
sparse LP, using Dantzig's pivot rule.

Usage procedure:
1. MAKE-SPARSE-LP
2. SPARSE-DUAL-PRIMAL!
3. SRARSE-LP-RESTORE

Reference:
Robert J. Vanderbei. Linear Programming: Foundations and Extensions. 5th edition."))
(in-package :cp/sparse-two-phase-simplex)

(defconstant +eps1+ (coerce 1d-8 'csc-float))
(defconstant +eps2+ (coerce 1d-12 'csc-float))
(defconstant +inf+ most-positive-double-float)

(defun pick-negative (vector)
  (declare (optimize (speed 3))
           ((simple-array csc-float (*)) vector))
  (let ((min (- +eps2+))
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
      (when (> (aref dx-values k) +eps1+)
        (let* ((index (aref dx-indices k))
               (rate (/ (aref x index) (aref dx-values k))))
          (when (< rate min)
            (setq min rate
                  res index)))))
    res))

(defun slp-primal! (sparse-lp)
  "Applies primal simplex method to SPARSE-LP and returns the terminal state:
:optimal or :unbounded."
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
:optimal or :infeasible."
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
