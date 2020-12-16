(defpackage :cp/test/integer-expression
  (:use :cl :fiveam :cp/integer-expression)
  (:import-from :cp/test/base #:base-suite))
(in-package :cp/test/integer-expression)
(in-suite base-suite)

(test integer-reverse*
  (is (= 1234560987 (integer-reverse* 1234567890 0 4)))
  (is (= 1234567890 (integer-reverse* 1234567890 0 1)))
  (is (= 1234587690 (integer-reverse* 1234567890 2 5)))
  (is (= 1234567890 (integer-reverse* 1234567890 7 7)))
  (is (= 21034567890 (integer-reverse* 1234567890 8 11)))
  (is (= #x1234587690 (integer-reverse* #x1234567890 2 5 16)))
  (is (= #x321004567890 (integer-reverse* #x1234567890 7 12 16))))
