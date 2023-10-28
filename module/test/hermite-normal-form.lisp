(defpackage :cp/test/hermite-normal-form
  (:use :cl :fiveam :cp/hermite-normal-form)
  (:import-from :cp/test/base #:base-suite)
  (:import-from :cp/mod-linear-algebra #:mod-echelon!)
  (:import-from :cp/gemm #:gemm))
(in-package :cp/test/hermite-normal-form)
(in-suite base-suite)

(defun calc-rank (mat)
  (destructuring-bind (m n) (array-dimensions  mat)
    (let ((tmp (make-array (list m n) :element-type '(signed-byte 32))))
      (dotimes (i m)
        (dotimes (j n)
          (setf (aref tmp i j) (aref mat i j))))
      (nth-value 1 (mod-echelon! tmp #.(+ 7 (expt 10 9)))))))

(test hnf-naive/hand
  (declare (notinline hnf-naive))
  (multiple-value-bind (h u) (hnf-naive #2a((5 0 0 0) (3 1 0 0) (1 0 19 0) (4 0 16 3)))
    (is (equalp h #2a((5 0 0 0) (0 1 0 0) (1 0 19 0) (1 0 1 3))))
    (is (equalp (gemm #2a((5 0 0 0) (3 1 0 0) (1 0 19 0) (4 0 16 3)) u) h)))
  (multiple-value-bind (h u) (hnf-naive #2a())
    (is (equalp h #2a()))
    (is (equalp u #2a())))
  (multiple-value-bind (h u) (hnf-naive (make-array '(0 2) :element-type 'fixnum))
    (is (equalp h (make-array '(0 2) :element-type 'fixnum)))
    (is (equalp u #2a((1 0) (0 1))))))

(test hnf-naive/random
  (let ((*random-state* (sb-ext:seed-random-state 0))
        (*test-dribble* nil))
    (dolist (magnitute '(5 50))
      (loop with trial = 0
            for m = (+ 1 (random 8))
            for n = (+ 1 (random 8))
            until (=  trial 20000)
            when (> m n)
            do (rotatef m n)
            do (let ((mat (make-array (list m n) :element-type t :initial-element 0)))
                 (dotimes (i m)
                   (dotimes (j n)
                     (setf (aref mat i j) (- (random (* 2 magnitute)) magnitute))))
                 (let ((rank (calc-rank mat)))
                   (when (= rank m)
                     (multiple-value-bind (h1 u1) (hnf-naive mat)
                       (let ((h2 (%hnf-full-rank mat)))
                         (is (equalp h1 (gemm mat u1)))
                         (is (equalp h1 h2))
                         (is (hnf-p h1))))
                     (incf trial))))))))

(deftype uint () '(integer 0 #.most-positive-fixnum))

(test %hnf-full-rank/hand
  (declare (notinline %hnf-full-rank))
  (is (equalp (%hnf-full-rank #2a((5 0 0 0) (3 1 0 0) (1 0 19 0) (4 0 16 3)))
              #2a((5 0 0 0) (0 1 0 0) (1 0 19 0) (1 0 1 3))))
  (is (equalp (%hnf-full-rank #2a()) #2a()))
  (is (equalp (%hnf-full-rank (make-array '(0 2) :element-type 'fixnum))
              (make-array '(0 2) :element-type 'fixnum))))

(test %gram-schmidt/hand
  (declare (notinline %gram-schmidt))
  ;; zero-size case
  (is (equalp (%gram-schmidt #2a())
              (make-gram-schmidt :matrix #2a()
                                 :coefs #2a()
                                 :rank 0
                                 :det2 1
                                 :basis-rows (make-array 0 :element-type 'uint)
                                 :row-multipliers #())))
  (is (equalp (%gram-schmidt (make-array '(0 3)))
              (make-gram-schmidt :matrix (make-array '(0 3))
                                 :coefs #2a()
                                 :rank 0
                                 :det2 1
                                 :basis-rows (make-array 0 :element-type 'uint)
                                 :row-multipliers #())))
  (is (equalp (%gram-schmidt #2a(() ()))
              (make-gram-schmidt :matrix #2a(() ())
                                 :coefs #2a(() ())
                                 :rank 0
                                 :det2 1
                                 :basis-rows (make-array 0 :element-type 'uint)
                                 :row-multipliers #(1 1))))
  ;; one-size case
  (is (equalp (%gram-schmidt #2a((0 0 0)))
              (make-gram-schmidt :matrix #2a((0 0 0))
                                 :coefs #2a(())
                                 :rank 0
                                 :det2 1
                                 :basis-rows (make-array 0 :element-type 'uint)
                                 :row-multipliers #(1))))
  (is (equalp (%gram-schmidt #2a((3 1)))
              (make-gram-schmidt :matrix #2a((3 1))
                                 :coefs #2a((1))
                                 :rank 1
                                 :det2 10
                                 :basis-rows (coerce #(0) '(simple-array uint (*)))
                                 :row-multipliers #(1))))
  (is (equalp (%gram-schmidt #2a((4) (0) (1)))
              (make-gram-schmidt :matrix #2a((4) (0) (0))
                                 :coefs #2a((1) (0) (4))
                                 :rank 1
                                 :det2 16
                                 :basis-rows (coerce #(0) '(simple-array uint (*)))
                                 :row-multipliers #(1 16 16))))
  ;; example from https://en.wikipedia.org/wiki/Gram%E2%80%93Schmidt_process
  (is (equalp (%gram-schmidt #2a((3 1) (2 2)))
              (make-gram-schmidt :matrix #2a((3 1) (-4 12))
                                 :coefs #2a((1 0) (-8 10)) ; (-4, 12) = -8(3, 1)+10(2, 2)
                                 :rank 2
                                 :det2 16
                                 :basis-rows (coerce #(0 1) '(simple-array uint (*)))
                                 :row-multipliers #(1 10))))
  ;; linearly dependent case
  (is (equalp (%gram-schmidt #2a((3 1 2) (9 3 6)))
              (make-gram-schmidt :matrix #2a((3 1 2) (0 0 0))
                                 :coefs #2a((1) (42))
                                 :rank 1
                                 :det2 14
                                 :basis-rows (coerce #(0) '(simple-array uint (*)))
                                 :row-multipliers #(1 14))))
  ;; row full rank case
  (is (equalp (%gram-schmidt #2a((1 1 1) (1 -1 2)))
              (make-gram-schmidt :matrix #2a((1 1 1) (1 -5 4))
                                 :coefs #2a((1 0) (-2 3))
                                 :rank 2
                                 :det2 14
                                 :basis-rows (coerce #(0 1) '(simple-array uint (*)))
                                 :row-multipliers #(1 3)))))

;; TODO: more effective test beyond just checking sanity
(test %gram-schmidt/random
  (let ((*random-state* (sb-ext:seed-random-state 0))
        (*test-dribble* nil))
    (dolist (magnitute '(5 76))
      (loop repeat 20000
            for m = (+ 1 (random 8))
            for n = (+ 1 (random 8))
            do (let ((orig-mat (make-array (list m n) :element-type t :initial-element 0)))
                 (dotimes (i m)
                   (dotimes (j n)
                     (setf (aref orig-mat i j) (- (random (* 2 magnitute)) magnitute))))
                 (let* ((res (%gram-schmidt orig-mat))
                        (rank (gram-schmidt-rank res))
                        (basis-rows (gram-schmidt-basis-rows res))
                        (gram-mat (gram-schmidt-matrix res))
                        (coefs (gram-schmidt-coefs res))
                        (det2 (gram-schmidt-det2 res))
                        (row-multipliers (gram-schmidt-row-multipliers res)))
                   (is (= rank (length basis-rows)))
                   ;; check the orthogonality
                   (dotimes (i rank)
                     (let ((i-row (aref basis-rows i)))
                       (loop for j from (+ i 1) below rank
                             for j-row = (aref basis-rows j)
                             do (is (zerop (loop for k below (array-dimension orig-mat 1)
                                                 sum (* (aref gram-mat i-row k)
                                                        (aref gram-mat j-row k))
                                                 of-type integer))))))
                   ;; check the consistency of the squared determinant of the lattice
                   (is (= det2
                          (loop with prod of-type integer = 1
                                for i below rank
                                for i-row = (aref basis-rows i)
                                do (setq prod
                                         (* prod
                                            (/ (loop for k below (array-dimension orig-mat 1)
                                                     sum (expt (aref gram-mat i-row k) 2))
                                               (expt (aref row-multipliers i-row) 2))))
                                finally (return prod))))
                   ;; restore original row vectors based on coefs
                   (dotimes (row m)
                     (let ((restored-vector (make-array n :element-type t :initial-element 0)))
                       (dotimes (i rank)
                         (let ((i-row (aref basis-rows i))
                               (coef (aref coefs row i)))
                           (unless (zerop coef)
                             (dotimes (col n)
                               (incf (aref restored-vector col)
                                     (* coef (aref orig-mat i-row col)))))))
                       (let ((target-vector
                               (coerce (if (find row basis-rows)
                                           (loop for col below n
                                                 collect (aref gram-mat row col))
                                           (loop with multiplier = (aref row-multipliers row)
                                                 for col below n
                                                 collect (* multiplier (aref orig-mat row col))))
                                       'simple-vector)))
                         (is (equalp restored-vector target-vector)))))))))))

(test %hnf-full-rank/random
  (let ((*random-state* (sb-ext:seed-random-state 0))
        (*test-dribble* nil))
    ;; Numbers that appear in the computation should be within fixnum because of
    ;; the Hadamard bound: sqrt(76^2 * 8)^8 ~ 4.56e18 < 4.61e18 ~ 2^62
    (dolist (magnitute '(5 76))
      (loop with trial = 0
            for m = (+ 1 (random 8))
            for n = (+ 1 (random 8))
            until (=  trial 20000)
            when (> m n)
            do (rotatef m n)
            do (let ((mat (make-array (list m n) :element-type 'fixnum :initial-element 0)))
                 (dotimes (i m)
                   (dotimes (j n)
                     (setf (aref mat i j) (- (random (* 2 magnitute)) magnitute))))
                 (let ((rank (calc-rank mat)))
                   (when (= rank m)
                     (let ((h (finishes (%hnf-full-rank mat))))
                       (is (hnf-p h)))
                     (incf trial))))))))

(defun hnf-test (mag)
  (declare (optimize (speed 3))
           ((integer 0 100) mag))
  (loop with trial of-type fixnum = 0
        for m = (+ 1 (random 10))
        for n = (+ 1 (random 10))
        until (=  trial 100000)
        when (> m n)
        do (rotatef m n)
        do (let ((mat (make-array (list m n) :element-type 'fixnum :initial-element 0)))
             (dotimes (i m)
               (dotimes (j n)
                 (setf (aref mat i j) (- (random (* 2 mag)) mag))))
             (let ((rank (calc-rank mat)))
               (declare (fixnum rank))
               (when (= rank m)
                 (let ((h (%hnf-full-rank mat)))
                   (hnf-p h))
                 (incf trial))))))

(defun hnf-single (mag size)
  (declare (optimize (speed 3))
           ((unsigned-byte 31) size mag))
  (let ((*random-state* (sb-ext:seed-random-state 0)))
    (loop with trial of-type fixnum = 0
          for m = size
          for n = size
          until (=  trial 1)
          do (let ((mat (make-array (list m n) :element-type t :initial-element 0)))
               (dotimes (i m)
                 (dotimes (j n)
                   (setf (aref mat i j) (- (random (* 2 mag)) mag))))
               (let ((rank (calc-rank mat)))
                 (declare (fixnum rank))
                 (when (= rank m)
                   (let ((h (%hnf-full-rank mat)))
                     (hnf-p h))
                   (incf trial)))))))
