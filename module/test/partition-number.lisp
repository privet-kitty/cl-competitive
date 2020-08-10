(eval-when (:compile-toplevel :load-toplevel :execute)
  (load "test-util")
  (load "../partition-number.lisp"))

(use-package :test-util)

(defconstant +mod+ 998244353)

(with-test (:name partition-number)
  (let ((table (make-partition-number-table 201 202 +mod+ :element-type '(unsigned-byte 32)))
        (seq (make-partition-number-sequence 201 +mod+)))
    (assert (= 1 (aref table 0 0)))
    (assert (= 1 (aref table 0 1)))
    (assert (= 0 (aref table 1 0)))
    (assert (= 5 (aref table 5 3)))
    (assert (= 7 (aref table 5 5)))
    (assert (= 7 (aref table 5 6)))
    (assert (= (mod 3972999029388 +mod+)
               (aref table 200 200)
               (aref table 200 201)))
    (assert (equalp '(unsigned-byte 32) (array-element-type table)))
    (loop for i from 0 to 200
          do (assert (= (aref table i i) (aref seq i))))))
