(defpackage :cp/test/lll
  (:use :cl :fiveam :cp/lll :cp/mod-linear-algebra :cp/copy-array :cp/hermite-normal-form)
  (:import-from :cp/test/base #:base-suite))
(in-package :cp/test/lll)
(in-suite base-suite)

(defun calc-rank (mat)
  (destructuring-bind (m n) (array-dimensions  mat)
    (let ((tmp (make-array (list m n) :element-type '(signed-byte 32))))
      (dotimes (i m)
        (dotimes (j n)
          (setf (aref tmp i j) (aref mat i j))))
      (nth-value 1 (mod-echelon! tmp #.(+ 7 (expt 10 9)))))))

(test lll-fractional/hand
  (is (equalp (lll-fractional #2a()) #2a()))
  (is (equalp (lll-fractional #2a((-3))) #2a((-3))))
  (is (equalp (lll-fractional #2a((0 1) (1 0))) #2a((0 1) (1 0))))
  (is (equalp (lll-fractional #2a((1 0) (0 1))) #2a((1 0) (0 1))))
  (is (equalp (lll-fractional #2a((4 1) (1 1))) #2a((1 2) (1 -1))))
  (is (equalp (lll-fractional #2a((1 4) (1 1))) #2a((1 2) (1 -1))))
  (signals division-by-zero (lll-fractional #2a((0))))
  (signals division-by-zero (lll-fractional #2a((6 4) (9 6))))
  (signals division-by-zero (lll-fractional #2a((1 2 3) (2 -1 3)))))

(test lll-fractional/random
  (let ((*random-state* (sb-ext:seed-random-state 0))
        (*test-dribble* nil))
    (dolist (magnitute '(5 50))
      (loop with trial = 0
            for m = (+ 1 (random 6))
            for n = (+ 1 (random 6))
            until (=  trial 10000)
            when (< m n)
            do (rotatef m n)
            do (let ((mat (make-array (list m n) :element-type t :initial-element 0)))
                 (dotimes (i m)
                   (dotimes (j n)
                     (setf (aref mat i j) (- (random (* 2 magnitute)) magnitute))))
                 (if (= (calc-rank mat) n)
                     (let ((reduced (lll-fractional mat)))
                       (is (equalp (hnf-matrix (hnf reduced))
                                   (hnf-matrix (hnf mat))))
                       (multiple-value-bind (ort coefs) (cp/lll::%rational-gram-schmidt reduced)
                         ;; Size reduction condtion
                         (dotimes (i n)
                           (dotimes (j n)
                             (is (<= (abs (aref coefs i j)) 1/2))))
                         ;; Lovász condition
                         (dotimes (k (- n 1))
                           (let ((l2 (loop for i below m
                                           sum (expt (aref ort i k) 2)))
                                 (l2-next (loop for i below m
                                                sum (expt (aref ort i (+ k 1)) 2))))
                             (is (<= (* 3/4 l2)
                                     (+ l2-next (* (expt (aref coefs (+ k 1) k) 2) l2)))))))
                       (incf trial))
                     (signals division-by-zero (lll-fractional mat))))))))

(test lll/hand
  (is (equalp (lll #2a()) #2a()))
  (is (equalp (lll #2a((-3))) #2a((-3))))
  (is (equalp (lll #2a((0 1) (1 0))) #2a((0 1) (1 0))))
  (is (equalp (lll #2a((1 0) (0 1))) #2a((1 0) (0 1))))
  (is (equalp (lll #2a((4 1) (1 1))) #2a((1 2) (1 -1))))
  (is (equalp (lll #2a((1 4) (1 1))) #2a((1 2) (1 -1))))
  (signals division-by-zero (lll-fractional #2a((0))))
  (signals division-by-zero (lll-fractional #2a((6 4) (9 6))))
  (signals division-by-zero (lll-fractional #2a((1 2 3) (2 -1 3)))))

(test lll/random
  (let ((*random-state* (sb-ext:seed-random-state 0))
        (*test-dribble* nil))
    (dolist (magnitute '(5 50))
      (loop with trial = 0
            for m = (+ 1 (random 6))
            for n = (+ 1 (random 6))
            until (=  trial 20000)
            when (< m n)
            do (rotatef m n)
            do (let ((mat (make-array (list m n) :element-type t :initial-element 0)))
                 (dotimes (i m)
                   (dotimes (j n)
                     (setf (aref mat i j) (- (random (* 2 magnitute)) magnitute))))
                 (if (= (calc-rank mat) n)
                     (let ((reduced (lll mat)))
                       (is (equalp (hnf-matrix (hnf reduced))
                                   (hnf-matrix (hnf mat))))
                       (multiple-value-bind (ort coefs) (cp/lll::%rational-gram-schmidt reduced)
                         ;; Size reduction condtion
                         (dotimes (i n)
                           (dotimes (j n)
                             (is (<= (abs (aref coefs i j)) 1/2))))
                         ;; Lovász condition
                         (dotimes (k (- n 1))
                           (let ((l2 (loop for i below m
                                           sum (expt (aref ort i k) 2)))
                                 (l2-next (loop for i below m
                                                sum (expt (aref ort i (+ k 1)) 2))))
                             (is (<= (* 3/4 l2)
                                     (+ l2-next (* (expt (aref coefs (+ k 1) k) 2) l2)))))))
                       (incf trial))
                     (signals division-by-zero (lll mat))))))))

(defun lll-single (mag size &optional (trial 1))
  (declare (optimize (speed 3))
           ((unsigned-byte 31) size mag))
  (let ((*random-state* (sb-ext:seed-random-state 0)))
    (loop repeat trial
          do (let ((mat (make-array (list size size) :element-type t :initial-element 0)))
               (dotimes (i size)
                 (dotimes (j size)
                   (setf (aref mat i j) (- (random (* 2 mag)) mag))))
               (assert (= (calc-rank mat) size))
               (lll mat)))))
