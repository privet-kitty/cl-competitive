(defpackage :cp/test/adjacent-duplicates
  (:use :cl :fiveam :cp/adjacent-duplicates)
  (:import-from :cp/test/base #:base-suite))
(in-package :cp/test/adjacent-duplicates)
(in-suite base-suite)

(test delete-adjacent-duplicates
  (is (equalp #() (delete-adjacent-duplicates #())))
  (is (equalp #(1 2 3 1 2) (delete-adjacent-duplicates (vector 1 2 2 3 3 1 1 1 2 2))))
  (is (equalp #(1 2 3) (delete-adjacent-duplicates (vector 1 1 2 3))))
  (is (equalp #(0 0.0 0) (delete-adjacent-duplicates (vector 0 0.0 0))))
  (is (equalp #(0) (delete-adjacent-duplicates (vector 0 0.0 0) :test #'=)))
  (is (equalp nil (delete-adjacent-duplicates nil)))
  (is (equalp '(1 2 3 1 2) (delete-adjacent-duplicates '(1 2 2 3 3 1 1 1 2 2))))
  (is (equalp '(1 2 3) (delete-adjacent-duplicates '(1 1 2 3))))
  (is (equalp '(0 0.0 0) (delete-adjacent-duplicates '(0 0.0 0))))
  (is (equalp '(0) (delete-adjacent-duplicates '(0 0.0 0) :test #'=))))
