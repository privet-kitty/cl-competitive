(defpackage :cp/test/adjacent-duplicates
  (:use :cl :fiveam :cp/adjacent-duplicates)
  (:import-from :cp/test/base #:base-suite))
(in-package :cp/test/adjacent-duplicates)
(in-suite base-suite)

(test delete-adjacent-duplicates
  (declare (notinline delete-adjacent-duplicates))
  ;; simple-vector
  (is (equalp #() (delete-adjacent-duplicates #())))
  (is (equalp #(1 2 3 1 2) (delete-adjacent-duplicates (vector 1 2 2 3 3 1 1 1 2 2))))
  (is (equalp #(1 2 3) (delete-adjacent-duplicates (vector 1 1 2 3))))
  (is (equalp #(1) (delete-adjacent-duplicates (vector 1))))
  (is (equalp #(0 0.0 0) (delete-adjacent-duplicates (vector 0 0.0 0))))
  (is (equalp #(0) (delete-adjacent-duplicates (vector 0 0.0 0) :test #'=)))
  ;; vector with fill-pointer
  (let ((vector (make-array 4 :fill-pointer 0)))
    (is (equalp #() (delete-adjacent-duplicates vector)))
    (vector-push-extend 3 vector)
    (vector-push-extend 3 vector)
    (vector-push-extend 4 vector)
    (is (equalp #(3 4) (delete-adjacent-duplicates vector)))
    (vector-push-extend 3 vector)
    (vector-push-extend 3 vector)
    (vector-push-extend 3 vector)
    (is (equalp #(3 4 3) (delete-adjacent-duplicates vector))))
  ;; list
  (is (equalp nil (delete-adjacent-duplicates nil)))
  (is (equalp '(1 2 3 1 2) (delete-adjacent-duplicates (list 1 2 2 3 3 1 1 1 2 2))))
  (is (equalp '(1 2 3) (delete-adjacent-duplicates (list 1 1 2 3))))
  (is (equalp '(1) (delete-adjacent-duplicates (list 1))))
  (is (equalp '(0 0.0 0) (delete-adjacent-duplicates (list 0 0.0 0))))
  (is (equalp '(0) (delete-adjacent-duplicates (list 0 0.0 0) :test #'=))))
