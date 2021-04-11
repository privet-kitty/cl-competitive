(defpackage :cp/test/alphametics
  (:use :cl :fiveam :cp/alphametics)
  (:import-from :cp/test/base #:base-suite))
(in-package :cp/test/alphametics)
(in-suite base-suite)

(test solve-alphametics
  (is (equalp '(9567 1085 10652)
              (multiple-value-list (solve-alphametics "send" "more" "money"))))
  (is (null (solve-alphametics "ab" "ab" "abcd"))))
