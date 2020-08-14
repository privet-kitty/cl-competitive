(defpackage :cp/test/partition-number
  (:use :cl :fiveam :cp/partition-number)
  (:import-from :cp/test/base #:base-suite))
(in-package :cp/test/partition-number)
(in-suite base-suite)

(defconstant +mod+ 998244353)

(test :name
  (let ((table (make-partition-number-table 201 202 +mod+ :element-type '(unsigned-byte 32)))
        (seq (make-partition-number-sequence 201 +mod+)))
    (is (= 1 (aref table 0 0)))
    (is (= 1 (aref table 0 1)))
    (is (= 0 (aref table 1 0)))
    (is (= 5 (aref table 5 3)))
    (is (= 7 (aref table 5 5)))
    (is (= 7 (aref table 5 6)))
    (is (= (mod 3972999029388 +mod+)
           (aref table 200 200)
           (aref table 200 201)))
    (is (equalp '(unsigned-byte 32) (array-element-type table)))
    (finishes
      (loop for i from 0 to 200
            do (assert (= (aref table i i) (aref seq i)))))))
