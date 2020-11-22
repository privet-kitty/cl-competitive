(defpackage :cp/test/subset-sum
  (:use :cl :fiveam :cp/subset-sum)
  (:import-from :cp/test/base #:base-suite))
(in-package :cp/test/subset-sum)
(in-suite base-suite)

(test subset-sum-merge
  (declare (notinline subset-sum-merge))
  (is (equalp #(0) (subset-sum-merge #(0) 0)))
  (is (equalp #(0 2) (subset-sum-merge #(0) 2)))
  (is (equalp #(0 2 4) (subset-sum-merge #(0 2) 2)))
  (is (equalp #(0 2 3 4 5 7) (subset-sum-merge #(0 2 4) 3)))
  (is (equalp #(0 2 4) (subset-sum-merge #(0 2 4) 0))))
