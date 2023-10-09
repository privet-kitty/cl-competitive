(defpackage :cp/test/hermite-normal-form
  (:use :cl :fiveam :cp/hermite-normal-form)
  (:import-from :cp/test/base #:base-suite)
  (:import-from :cp/mod-linear-algebra #:mod-echelon!)
  (:import-from :cp/gemm #:gemm))
(in-package :cp/test/hermite-normal-form)
(in-suite base-suite)

(test hnf!/hand
  (let ((mat (make-array '(4 4)
                         :element-type 'fixnum
                         :initial-contents '((5 0 0 0) (3 1 0 0) (1 0 19 0) (4 0 16 3)))))
    (multiple-value-bind (h u) (hnf! mat)
      (is (equalp h #2a((5 0 0 0) (0 1 0 0) (1 0 19 0) (1 0 1 3))))
      (is (equalp (gemm #2a((5 0 0 0) (3 1 0 0) (1 0 19 0) (4 0 16 3)) u) h)))))


(defun calc-rank (mat)
  (destructuring-bind (m n) (array-dimensions  mat)
    (let ((tmp (make-array (list m n) :element-type '(signed-byte 32))))
      (dotimes (i m)
        (dotimes (j n)
          (setf (aref tmp i j) (aref mat i j))))
      (nth-value 1 (mod-echelon! tmp #.(+ 7 (expt 10 9)))))))

(test hnf!/random
  (let ((*random-state* (sb-ext:seed-random-state 0))
        (*test-dribble* nil))
    (dolist (magnitute '(10 100))
      (loop with trial = 0
            for m = (+ 1 (random 10))
            for n = (+ 1 (random 10))
            until (=  trial 5000)
            when (> m n)
            do (rotatef m n)
            do (let ((mat (make-array (list m n) :element-type t :initial-element 0)))
                 (dotimes (i m)
                   (dotimes (j n)
                     (setf (aref mat i j) (- (random (* 2 magnitute)) magnitute))))
                 (let ((rank (calc-rank mat)))
                   (when (= rank m)
                     (let ((orig (make-array (list m n) :element-type t :initial-element 0)))
                       (dotimes (i m)
                         (dotimes (j n)
                           (setf (aref orig i j) (aref mat i j))))
                       (multiple-value-bind (h u) (hnf! mat)
                         (is (equalp h (gemm orig u)))
                         (is (hnf-p h))))
                     (incf trial))))))))
