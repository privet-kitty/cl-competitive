(defpackage :cp/test/lp
  (:use :cl :fiveam :cp/lp :cp/csc)
  (:import-from :cp/test/base #:base-suite)
  (:import-from :cp/lp #:to-standard-ab #:to-standard-c))
(in-package :cp/test/lp)
(in-suite base-suite)

(defun to-standard-form (problem)
  (multiple-value-bind (a b) (to-standard-ab problem)
    (multiple-value-bind (c obj-offset) (to-standard-c problem)
      (values (csc-to-array a) b c obj-offset))))

(test to-standard-form/hand
  ;; empty problem
  (let ((problem (make-lp-problem :sense -1)))
    (multiple-value-bind (a b c offset) (to-standard-form problem)
      (is (equalp a #2a()))
      (is (equalp b #()))
      (is (equalp c #()))
      (is (= offset 0d0))))
  ;; max. x1 + x2 s. t. x1 + 2x2 free, 30 <= 2x1 + x2, x1 free, x2 <= 10
  (let* ((problem (make-lp-problem))
         (xs (vector (new-lp-var problem nil nil)
                     (new-lp-var problem nil 10d0)))
         (constr (vector (new-lp-constr problem
                                        (make-linear-expr '(1d0 2d0) xs)
                                        nil nil)
                         (new-lp-constr problem
                                        (make-linear-expr '(2d0 1d0) xs)
                                        30d0 nil))))
    (declare (ignore constr))
    (setf (lp-problem-objective problem)
          (make-linear-expr '(1d0 1d0) xs))
    (multiple-value-bind (a b c offset) (to-standard-form problem)
      (is (equalp a #2a((2d0 -2d0 -1d0 -1d0))))
      (is (equalp b #(20d0)))
      (is (equalp c #(1d0 -1d0 -1d0 0d0)))
      (is (= offset 10d0))))
  ;; min. 3x1-x2
  ;; s. t. 1 <= -x1+x2 <= 5,
  ;; 2 <= -3x1+2x2 <= 10,
  ;; 2x1-x2 <= 0,
  ;; -2 <= x1,
  ;; 0 <= x2 <= 6
  (let* ((problem (make-lp-problem :sense -1))
         (xs (vector (new-lp-var problem -2d0 nil "x1")
                     (new-lp-var problem 0d0 6d0 "x2")))
         (constr (vector (new-lp-constr problem
                                        (make-linear-expr '(-1d0 1d0) xs)
                                        1d0 5d0)
                         (new-lp-constr problem
                                        (make-linear-expr '(-3d0 2d0) xs)
                                        2d0 10d0)
                         (new-lp-constr problem
                                        (make-linear-expr '(2d0 -1d0) xs)
                                        nil 0d0))))
    (declare (ignore constr))
    (setf (lp-problem-objective problem)
          (make-linear-expr '(3d0 -1d0) xs))
    (multiple-value-bind (a b c obj-offset) (to-standard-form problem)
      (is (equalp a #2a((0.0d0 1.0d0 1.0d0 0.0d0 0.0d0 0.0d0 0.0d0 0.0d0)
                        (-1.0d0 1.0d0 0.0d0 -1.0d0 0.0d0 0.0d0 0.0d0 0.0d0)
                        (0.0d0 0.0d0 0.0d0 1.0d0 1.0d0 0.0d0 0.0d0 0.0d0)
                        (-3.0d0 2.0d0 0.0d0 0.0d0 0.0d0 -1.0d0 0.0d0 0.0d0)
                        (0.0d0 0.0d0 0.0d0 0.0d0 0.0d0 1.0d0 1.0d0 0.0d0)
                        (2.0d0 -1.0d0 0.0d0 0.0d0 0.0d0 0.0d0 0.0d0 1.0d0))))
      (is (equalp b #(6.0d0 -1.0d0 4.0d0 -4.0d0 8.0d0 4.0d0)))
      (is (equalp c #(-3.0d0 1.0d0 0.0d0 0.0d0 0.0d0 0.0d0 0.0d0 0.0d0)))
      (is (= obj-offset 6.0d0)))))
