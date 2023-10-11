(defpackage :cp/test/bareiss
  (:use :cl :fiveam :cp/bareiss)
  (:import-from :cp/test/base #:base-suite)
  (:import-from :cp/copy-array #:copy-array)
  (:import-from :cp/mod-linear-algebra #:mod-echelon! #:mod-determinant!))
(in-package :cp/test/bareiss)
(in-suite base-suite)

(defconstant +mod+ (+ 7 (expt 10 9)))

(test bareiss!/hand
  (declare (notinline bareiss!))
  (multiple-value-bind (det rank cols) (bareiss! (copy-array #2a()))
    (is (= det 1))
    (is (= rank 0))
    (is (equalp cols #())))
  (multiple-value-bind (det rank cols) (bareiss! (make-array '(0 2) :initial-element 0))
    (is (= det 1))
    (is (= rank 0))
    (is (equalp cols #())))
  (multiple-value-bind (det rank cols) (bareiss! #2a((1 1 0) (0 0 -2)))
    (is (= det -2))
    (is (= rank 2))
    (is (equalp cols #(0 2))))
  (multiple-value-bind (det rank cols) (bareiss! #2a((1 1 1) (0 0 0)))
    (is (= det 0))
    (is (= rank 1))
    (is (equalp cols #(0)))))

(defun calc-rank (mat)
  (declare (optimize (speed 3))
           ((simple-array fixnum (* *)) mat))
  (destructuring-bind (m n) (array-dimensions  mat)
    (let ((tmp (make-array (list m n) :element-type '(unsigned-byte 31))))
      (dotimes (i m)
        (dotimes (j n)
          (setf (aref tmp i j) (mod (aref mat i j) +mod+))))
      (nth-value 1 (mod-echelon! tmp +mod+)))))

(defun calc-det (mat cols)
  (declare (optimize (speed 3))
           ((simple-array fixnum (* *)) mat)
           ((simple-array (integer 0 #.most-positive-fixnum) (*)) cols))
  (let ((m (array-dimension mat 0)))
    (unless (= m (length cols))
      (return-from calc-det 0))
    (let ((rec-mat (make-array (list m m) :element-type '(unsigned-byte 31))))
      (dotimes (i m)
        (dotimes (k m)
          (setf (aref rec-mat i k) (mod (aref mat i (aref cols k)) +mod+))))
      (mod-determinant! rec-mat +mod+))))

(test bareiss!/random
  (let ((*random-state* (sb-ext:seed-random-state 0))
        (*test-dribble* nil))
    (dolist (mag '(5 50))
      (loop for trial below 50000
            for m = (+ 1 (random 8))
            for n = (+ 1 (random 8))
            when (> m n)
            do (rotatef m n)
            do (let ((mat (make-array (list m n) :element-type 'fixnum)))
                 (dotimes (i m)
                   (dotimes (j n)
                     (setf (aref mat i j) (- (random (* 2 mag)) mag))))
                 (multiple-value-bind (det rank cols) (bareiss! (copy-array mat))
                   (is (= rank (calc-rank mat)))
                   (is (= (mod det +mod+) (calc-det mat cols)))
                   (is (= rank (length cols)))
                   (if (= rank m)
                       (is-false (zerop det))
                       (is-true (zerop det)))))))))
