(defpackage :cp/test/sparse-two-phase-simplex
  (:use :cl :fiveam :cp/sparse-two-phase-simplex :cp/test/nearly-equal
        :cp/csc :cp/lu-decomposition :cp/lp-test-tool)
  (:import-from :cp/test/base #:base-suite)
  (:import-from :cp/sparse-two-phase-simplex #:tmat-times-vec!))
(in-package :cp/test/sparse-two-phase-simplex)
(in-suite base-suite)

(test tmat-times-vec!
  (let* ((vec (make-sparse-vector-from #(1d0 0d0 2d0 0d0 -1d0)))
         (tmat (make-csc-from-array #2a((1d0 2d0 3d0 4d0 7d0)
                                        (6d0 7d0 8d0 9d0 10d0)
                                        (11d0 12d0 13d0 14d0 15d0)
                                        (16d0 17d0 18d0 19d00 20d0)
                                        (21d0 22d0 23d0 24d0 25d0)
                                        (100d0 0d0 100d0 1d0 1d0))))
         (res (make-sparse-vector 5))
         (basic-flag (coerce '(-1 -2 -4 -5 0 -3) '(simple-array fixnum (*))))
         (dense (sparse-vector-to-dense (tmat-times-vec! tmat vec basic-flag res))))
    (is (nearly-equalp 1d-8 #(0d0 12d0 299d0 22d0 32d0) dense)))
  (is (equalp #()
              (sparse-vector-to-dense
               (tmat-times-vec! (make-csc-from-array #2a())
                                (make-sparse-vector 0)
                                (make-array 0 :element-type 'fixnum)
                                (make-sparse-vector 0))))))

#+swank (set-dispatch-macro-character #\# #\> #'cl-debug-print:debug-print-reader)

(defmacro dbg (&rest forms)
  (declare (ignorable forms))
  #+swank (if (= (length forms) 1)
              `(format *error-output* "~A => ~A~%" ',(car forms) ,(car forms))
              `(format *error-output* "~A => ~A~%" ',forms `(,,@forms))))

(test sparse-primal!
  ;; trival lp
  (let* ((b (make-array 0 :element-type 'double-float))
         (c (make-array 0 :element-type 'double-float))
         (lp (make-sparse-lp (make-csc-from-array #2a()) b c)))
    (is (eql :optimal (sparse-primal! lp)))
    (multiple-value-bind (obj prim dual prim-slack dual-slack) (sparse-lp-restore lp)
      (is (zerop obj))
      (is (equalp #() prim))
      (is (equalp #() dual))
      (is (equalp #() prim-slack))
      (is (equalp #() dual-slack))))
  ;; zero lp
  (dolist (dim '((2 . 3) (10 . 10) (1 . 1) (10 . 3) (3 . 10)))
    (destructuring-bind (m . n) dim
      (let* ((mat (make-array (list m n) :element-type 'double-float :initial-element 0d0))
             (b (make-array m :element-type 'double-float :initial-element 0d0))
             (c+ (make-array n :element-type 'double-float :initial-element 1d0))
             (c- (make-array n :element-type 'double-float :initial-element -1d0))
             (csc (make-csc-from-array mat)))
        (dotimes (i m)
          (setf (aref b i) (float (random 20) 1d0)))
        (let* ((lp (make-sparse-lp csc b c+)))
          (is (eql :unbounded (sparse-primal! lp))))
        (let* ((lp (make-sparse-lp csc b c-)))
          (is (eql :optimal (sparse-primal! lp))))))))

(test sparse-one-phase-simplex/random
  (let ((*random-state* (sb-ext:seed-random-state 0))
        (*test-dribble* nil))
    (labels
        ((proc ()
           (dolist (dim '((2 . 3) (10 . 10) (1 . 1) (10 . 3) (3 . 10)))
             (dolist (rate '(0.1 0.4 0.7 0.9))
               (dotimes (_ 200)
                 (let* ((m (car dim))
                        (n (cdr dim))
                        (mat (make-array (list m n)
                                         :element-type 'double-float
                                         :initial-element 0d0))
                        (mat-dual (make-array (list n m)
                                              :element-type 'double-float
                                              :initial-element 0d0))
                        
                        (b (make-array m :element-type 'double-float :initial-element 0d0))
                        (c (make-array n :element-type 'double-float :initial-element 0d0)))
                   (dotimes (i m)
                     (dotimes (j n)
                       (when (< (random 1d0) rate)
                         (let ((val (float (- (random 20) 10) 1d0)))
                           (setf (aref mat i j) val
                                 (aref mat-dual j i) (- val))))))
                   (dotimes (i m)
                     (setf (aref b i) (float (random 20) 1d0)))
                   (dotimes (j n)
                     (setf (aref c j) (float (- (random 20) 10) 1d0)))
                   (let* ((b- (map '(simple-array double-float (*)) #'- b))
                          (c- (map '(simple-array double-float (*)) #'- c))
                          (csc (make-csc-from-array mat))
                          (csc-dual (make-csc-from-array mat-dual))
                          (lp (make-sparse-lp csc b c))
                          (lp-dual (make-sparse-lp csc-dual c- b-))
                          (result1 (sparse-primal! lp))
                          (result2 (sparse-dual! lp-dual)))
                     (is (or (and (eql result1 :optimal) (eql result2 :optimal))
                             (and (eql result1 :unbounded) (eql result2 :infeasible))))
                     ;; check sparse-primal!
                     (when (eql result1 :optimal)
                       (multiple-value-bind (obj prim dual prim-slack dual-slack)
                           (sparse-lp-restore lp)
                         ;; check primal
                         (let ((lhs (csc-gemv csc prim))
                               (obj2 (loop for x across prim
                                           for coef across c
                                           sum (* x coef))))
                           (is (nearly= 1d-8 obj obj2))
                           (dotimes (i (length prim-slack))
                             (incf (aref lhs i) (aref prim-slack i)))
                           (is (nearly-equalp 1d-8 lhs b))
                           (is (loop for x across prim
                                     always (>= x -1d-8)))
                           (is (loop for x across prim-slack
                                     always (>= x -1d-8))))
                         ;; check dual
                         (let ((lhs (csc-gemv csc-dual dual))
                               (obj2 (loop for y across dual
                                           for coef across b-
                                           sum (* y coef))))
                           (dotimes (i (length dual-slack))
                             (incf (aref lhs i) (aref dual-slack i)))
                           (is (nearly= 1d-8 obj (- obj2)))
                           (is (nearly-equalp 1d-8 lhs c-))
                           (is (loop for x across dual
                                     always (>= x -1d-8)))
                           (is (loop for x across dual-slack
                                     always (>= x -1d-8))))))
                     ;; check sparse-dual!
                     (when (eql result2 :optimal)
                       (multiple-value-bind (obj prim dual prim-slack dual-slack)
                           (sparse-lp-restore lp-dual)
                         ;; check primal
                         (let ((lhs (csc-gemv csc-dual prim))
                               (obj2 (loop for x across prim
                                           for coef across b-
                                           sum (* x coef))))
                           (is (nearly= 1d-8 obj obj2))
                           (dotimes (i (length prim-slack))
                             (incf (aref lhs i) (aref prim-slack i)))
                           (is (nearly-equalp 1d-8 lhs c-))
                           (is (loop for x across prim
                                     always (>= x -1d-8)))
                           (is (loop for x across prim-slack
                                     always (>= x -1d-8))))
                         ;; check dual
                         (let ((lhs (csc-gemv csc dual))
                               (obj2 (loop for y across dual
                                           for coef across c
                                           sum (* y coef))))
                           (dotimes (i (length dual-slack))
                             (incf (aref lhs i) (aref dual-slack i)))
                           (is (nearly= 1d-8 obj (- obj2)))
                           (is (nearly-equalp 1d-8 lhs b))
                           (is (loop for x across dual
                                     always (>= x -1d-8)))
                           (is (loop for x across dual-slack
                                     always (>= x -1d-8)))))))))))))
      (let ((*refactor-threshold* 1))
        (proc))
      (let ((*refactor-threshold* 200)
            (*refactor-by-time* nil))
        (proc)))))

(defun test* ()
  (let ((m 500)
        (n 400)
        (rate 0.05d0)
        (*random-state* (sb-ext:seed-random-state 0)))
    (dotimes (_ 5)
      (let ((coo (make-coo m n))
            (b (make-array m :element-type 'double-float :initial-element 0d0))
            (c (make-array n :element-type 'double-float :initial-element 0d0)))
        (dotimes (i m)
          (dotimes (j n)
            (when (< (random 1d0) rate)
              (let ((val (float (- (random 20) 10) 1d0)))
                (coo-insert! coo i j val)))))
        (dotimes (i m)
          (setf (aref b i) (float (random 20) 1d0)))
        (dotimes (j n)
          (setf (aref c j) (float (- (random 20) 10) 1d0)))
        (let* ((csc (make-csc-from-coo coo))
               (lp (make-sparse-lp csc b c))
               (result (sparse-primal! lp)))
          (print result)
          (if (eql result :optimal)
              (multiple-value-bind (obj prim dual prim-slack dual-slack)
                  (sparse-lp-restore lp)
                ;; check primal
                (let ((lhs (csc-gemv csc prim))
                      (obj2 (loop for x across prim
                                  for coef across c
                                  sum (* x coef))))
                  (assert (nearly= 1d-5 obj obj2))
                  (dotimes (i (length prim-slack))
                    (incf (aref lhs i) (aref prim-slack i)))
                  (assert (nearly-equalp 1d-5 lhs b))
                  (assert (loop for x across prim
                                always (>= x -1d-5)))
                  (assert (loop for x across prim-slack
                                always (>= x -1d-5)))))))))))
