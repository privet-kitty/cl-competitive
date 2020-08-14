(defpackage :cp/test/2d-sparse-table
  (:use :cl :fiveam :cp/2d-sparse-table)
  (:import-from :cp/test/base #:base-suite))
(in-package :cp/test/2d-sparse-table)
(in-suite base-suite)

(test 2d-sparse-table/manual
  (declare (notinline make-2d-sparse-table))
  (is (equal '(1 1 0 0)
             (array-dimensions (make-2d-sparse-table #2a() #'min))))
  (let ((table (make-2d-sparse-table #2a((2 1 1 9 6)
                                         (8 5 6 2 5)
                                         (1 6 2 3 7)
                                         (0 9 5 9 0))
                                     #'max)))
    (is (= 35 (2dst-query table #'max 0 1 0 4 35)))
    (is (= 35 (2dst-query table #'max 1 3 2 3 35)))
    (is (= 9 (2dst-query table #'max 0 0 4 5)))
    (is (= 6 (2dst-query table #'max 1 2 2 3)))
    (is (= 6 (2dst-query table #'max 1 2 3 4)))
    (is (= 7 (2dst-query table #'max 1 2 3 5)))))

(defun query-max (matrix i1 j1 i2 j2 identity)
  (declare ((simple-array fixnum (* *)) matrix)
           (fixnum i1 j1 i2 j2 identity))
  (let ((res identity))
    (declare (fixnum res))
    (loop for i from i1 below i2
          do (loop for j from j1 below j2
                   do (setq res (max res (aref matrix i j)))))
    res))

(defparameter *state* (sb-ext:seed-random-state 0))

(defun make-random-matrix (m n)
  (let ((res (make-array (list m n) :element-type 'fixnum)))
    (dotimes (i m)
      (dotimes (j n)
        (setf (aref res i j) (- (random 2000 *state*) 1000))))
    res))

(test 2d-sparse-table/random
  (finishes
    (dolist (dim '((7 . 17) (4 . 8) (2 . 1) (11 . 5)))
      (dotimes (_ 100)
        (let* ((m (car dim))
               (n (cdr dim))
               (matrix (make-random-matrix m n))
               (table (make-2d-sparse-table matrix #'max)))
          (loop
            for i1 from 0 to m
            do (loop
                 for j1 from 0 to n
                 do (loop
                      for i2 from i1 to m
                      do (loop
                           for j2 from j1 to n
                           do (assert (= (2dst-query table #'max i1 j1 i2 j2 -12345)
                                         (query-max matrix i1 j1 i2 j2 -12345))))))))))))
