(defpackage :cp/test/sublist
  (:use :cl :fiveam :cp/sublist)
  (:import-from :cp/test/base #:base-suite))
(in-package :cp/test/sublist)
(in-suite base-suite)

(test sublist!
  (is (null (sublist! nil 0 0)))
  (is (null (sublist! nil 0)))
  (is (equal '(1 2 3 4) (sublist! (list 1 2 3 4) 0)))
  (is (equal '(2 3 4) (sublist! (list 1 2 3 4) 1)))
  (is (equal '(2 3) (sublist! (list 1 2 3 4) 1 3)))
  (is (null (sublist! (list 1 2 3 4) 0 0)))
  (is (null (sublist! (list 1 2 3 4) 4 4))))
