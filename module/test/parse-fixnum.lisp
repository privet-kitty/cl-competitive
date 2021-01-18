(defpackage :cp/test/parse-fixnum
  (:use :cl :fiveam :cp/parse-fixnum)
  (:import-from :cp/test/base #:base-suite))
(in-package :cp/test/parse-fixnum)
(in-suite base-suite)

(test parse-fixnum
  (is (= 1234567890987654321 (parse-fixnum " 1234567890987654321")))
  (is (= -1234567890987654321 (parse-fixnum "-1234567890987654321")))
  (is (= 1234567890987654321 (parse-fixnum "+1234567890987654321")))
  (is (= 234 (parse-fixnum " 1234567890" :start 2 :end 5)))
  (let ((*test-dribble* nil))
    (loop for x from -100 to 100
          do (is (= x (parse-fixnum (write-to-string x)))))))
