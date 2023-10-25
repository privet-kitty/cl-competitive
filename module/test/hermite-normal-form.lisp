(defpackage :cp/test/hermite-normal-form
  (:use :cl :fiveam :cp/hermite-normal-form)
  (:import-from :cp/test/base #:base-suite)
  (:import-from :cp/mod-linear-algebra #:mod-echelon!)
  (:import-from :cp/gemm #:gemm)
  (:import-from :cp/copy-array #:copy-array))
(in-package :cp/test/hermite-normal-form)
(in-suite base-suite)

(defun calc-rank (mat)
  (destructuring-bind (m n) (array-dimensions  mat)
    (let ((tmp (make-array (list m n) :element-type '(signed-byte 32))))
      (dotimes (i m)
        (dotimes (j n)
          (setf (aref tmp i j) (aref mat i j))))
      (nth-value 1 (mod-echelon! tmp #.(+ 7 (expt 10 9)))))))

(test hnf!/hand
  (declare (notinline hnf!))
  (let ((mat #2a((5 0 0 0) (3 1 0 0) (1 0 19 0) (4 0 16 3))))
    (multiple-value-bind (h u) (hnf! (copy-array mat))
      (is (equalp h #2a((5 0 0 0) (0 1 0 0) (1 0 19 0) (1 0 1 3))))
      (is (equalp (gemm #2a((5 0 0 0) (3 1 0 0) (1 0 19 0) (4 0 16 3)) u) h))))
  (multiple-value-bind (h u) (hnf! (copy-array #2a()))
    (is (equalp h #2a()))
    (is (equalp u #2a())))
  (multiple-value-bind (h u) (hnf! (make-array '(0 2) :element-type 'fixnum))
    (is (equalp h (make-array '(0 2) :element-type 'fixnum)))
    (is (equalp u #2a((1 0) (0 1))))))

(test hnf!/random
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
                     (multiple-value-bind (h1 u1) (hnf! (copy-array mat))
                       (let ((h2 (%hnf-fast! (copy-array mat))))
                         (is (equalp h1 (gemm mat u1)))
                         (is (equalp h1 h2))
                         (is (hnf-p h1))))
                     (incf trial))))))))

(deftype uint () '(integer 0 #.most-positive-fixnum))

(test %hnf-fast!/hand
  (declare (notinline %hnf-fast!))
  (let* ((mat #2a((5 0 0 0) (3 1 0 0) (1 0 19 0) (4 0 16 3)))
         (h (%hnf-fast! (copy-array mat))))
    (is (equalp h #2a((5 0 0 0) (0 1 0 0) (1 0 19 0) (1 0 1 3)))))
  (is (equalp (%hnf-fast! (copy-array #2a())) #2a()))
  (is (equalp (%hnf-fast! (make-array '(0 2) :element-type 'fixnum))
              (make-array '(0 2) :element-type 'fixnum))))

(test %gram-schmidt!/hand
  ;; zero-size
  (is (equalp (%gram-schmidt! (make-array '(0 0)))
              (make-gram-schmidt :matrix #2a()
                                 :rank 0
                                 :det2 1
                                 :basis-rows (make-array 0 :element-type 'uint)
                                 :row-magnifiers #())))
  (is (equalp (%gram-schmidt! (make-array '(0 3)))
              (make-gram-schmidt :matrix (make-array '(0 3))
                                 :rank 0
                                 :det2 1
                                 :basis-rows (make-array 0 :element-type 'uint)
                                 :row-magnifiers #())))
  (is (equalp (%gram-schmidt! (make-array '(2 0)))
              (make-gram-schmidt :matrix (make-array '(2 0))
                                 :rank 0
                                 :det2 1
                                 :basis-rows (make-array 0 :element-type 'uint)
                                 :row-magnifiers #(1 1))))
  ;; one-size
  (is (equalp (%gram-schmidt! (copy-array #2a((0 0 0))))
              (make-gram-schmidt :matrix #2a((0 0 0))
                                 :rank 0
                                 :det2 1
                                 :basis-rows (make-array 0 :element-type 'uint)
                                 :row-magnifiers #(1))))
  (is (equalp (%gram-schmidt! (copy-array #2a((3 1))))
              (make-gram-schmidt :matrix #2a((3 1))
                                 :rank 1
                                 :det2 10
                                 :basis-rows (coerce #(0) '(simple-array uint (*)))
                                 :row-magnifiers #(1))))
  (is (equalp (%gram-schmidt! (copy-array #2a((4) (0) (1))))
              (make-gram-schmidt :matrix #2a((4) (0) (0))
                                 :rank 1
                                 :det2 16
                                 :basis-rows (coerce #(0) '(simple-array uint (*)))
                                 :row-magnifiers #(1 16 16))))
  ;; https://en.wikipedia.org/wiki/Gram%E2%80%93Schmidt_process
  (is (equalp (%gram-schmidt! (copy-array #2a((3 1) (2 2))))
              (make-gram-schmidt :matrix #2a((3 1) (-4 12))
                                 :rank 2
                                 :det2 16
                                 :basis-rows (coerce #(0 1) '(simple-array uint (*)))
                                 :row-magnifiers #(1 10))))
  ;; dependent
  (is (equalp (%gram-schmidt! (copy-array #2a((3 1 2) (9 3 6))))
              (make-gram-schmidt :matrix #2a((3 1 2) (0 0 0))
                                 :rank 1
                                 :det2 14
                                 :basis-rows (coerce #(0) '(simple-array uint (*)))
                                 :row-magnifiers #(1 14))))
  ;; row full rank
  (is (equalp (%gram-schmidt! (copy-array #2a((1 1 1) (1 -1 2))))
              (make-gram-schmidt :matrix #2a((1 1 1) (1 -5 4))
                                 :rank 2
                                 :det2 14
                                 :basis-rows (coerce #(0 1) '(simple-array uint (*)))
                                 :row-magnifiers #(1 3)))))

;; TODO: more effective test beyond just checking sanity
(test %gram-schmidt!/random
  (let ((*random-state* (sb-ext:seed-random-state 0))
        (*test-dribble* nil))
    (dolist (magnitute '(5 76))
      (loop repeat 20000
            for m = (+ 1 (random 8))
            for n = (+ 1 (random 8))
            do (let ((mat (make-array (list m n) :element-type t :initial-element 0)))
                 (dotimes (i m)
                   (dotimes (j n)
                     (setf (aref mat i j) (- (random (* 2 magnitute)) magnitute))))
                 (let* ((res (%gram-schmidt! (copy-array mat)))
                        (rank (gram-schmidt-rank res))
                        (basis-rows (gram-schmidt-basis-rows res))
                        (gram-mat (gram-schmidt-matrix res))
                        (det2 (gram-schmidt-det2 res))
                        (row-magnifiers (gram-schmidt-row-magnifiers res)))
                   (is (= rank (length basis-rows)))
                   ;; orthogonality
                   (dotimes (i rank)
                     (let ((i-row (aref basis-rows i)))
                       (loop for j from (+ i 1) below rank
                             for j-row = (aref basis-rows j)
                             do (is (zerop (loop for k below (array-dimension mat 1)
                                                 sum (* (aref gram-mat i-row k)
                                                        (aref gram-mat j-row k))
                                                 of-type integer))))))
                   ;; squared determinant of lattice
                   (is (= det2
                          (loop with prod of-type integer = 1
                                for i below rank
                                for i-row = (aref basis-rows i)
                                do (setq prod
                                         (* prod
                                            (/ (loop for k below (array-dimension mat 1)
                                                     sum (expt (aref gram-mat i-row k) 2))
                                               (expt (aref row-magnifiers i-row) 2))))
                                finally (return prod))))))))))

(test %hnf-fast!/random
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
                     (let ((h (finishes (%hnf-fast! mat))))
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
                 (let ((h (%hnf-fast! mat)))
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
                   (let ((h (%hnf-fast! mat)))
                     (hnf-p h))
                   (incf trial)))))))
