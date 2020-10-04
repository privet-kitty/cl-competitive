(defpackage :cp/test/next-table
  (:use :cl :fiveam :cp/next-table)
  (:import-from :cp/test/base #:base-suite))
(in-package :cp/test/next-table)
(in-suite base-suite)

(test next-table
  (is (equalp #(2 1 0 3) (make-next-table #(3 2 3 1))))
  (is (equalp #(2 5 4 7) (make-next-table #(3 2 3 1) :double t)))
  (is (equalp #(0 1 2 3) (make-next-table (vector (list 3) (list 2) (list 3) (list 1)))))
  (is (equalp #(2 1 0 3) (make-next-table (vector (list 3) (list 2) (list 3) (list 1))
                                          :test #'equal))))
