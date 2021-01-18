(defpackage :cp/test/parse-bignum
  (:use :cl :fiveam :cp/parse-bignum)
  (:import-from :cp/test/base #:base-suite))
(in-package :cp/test/parse-bignum)
(in-suite base-suite)

(test parse-bignum
  (is (= 12345678909876543210123456789 (parse-bignum " 12345678909876543210123456789")))
  (is (= -12345678909876543210123456789 (parse-bignum "-12345678909876543210123456789")))
  (is (= 12345678909876543210123456789 (parse-bignum "+12345678909876543210123456789")))
  (is (= 234 (parse-bignum " 1234567890" :start 2 :end 5)))
  (let ((*test-dribble* nil))
    (loop for x from -100 to 100
          do (is (= x (parse-bignum (write-to-string x)))))))
