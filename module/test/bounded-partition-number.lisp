(defpackage :cp/test/bounded-partition-number
  (:use :cl :fiveam :cp/bounded-partition-number)
  (:import-from :cp/test/base #:base-suite))
(in-package :cp/test/bounded-partition-number)
(in-suite base-suite)

(test make-bpartition-table
  (let ((table (make-bpartition-table 1 0 100000)))
    (is (= 1 (aref table 0 0)))
    (is (equal '(1 1) (array-dimensions table))))
  (let ((table (make-bpartition-table 361 25 100000)))
    (is (= 74501 (aref table 360 25)))
    (is (= (aref table 3 3) (aref table 3 4) (aref table 3 5)))
    (is (= 0 (aref table 3 0)))
    (is (= 1 (aref table 0 0)))
    (is (= 1 (aref table 0 1)))))
