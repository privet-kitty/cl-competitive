(defpackage :cp/test/compress
  (:use :cl :fiveam :cp/compress)
  (:import-from :cp/test/base #:base-suite))
(in-package :cp/test/compress)
(in-suite base-suite)

(test compress!
  (is (equalp #(#\a #\c #\e) (compress! (vector #\c #\c #\e #\a #\e #\c) #'char<)))
  (is (equalp #() (compress! #() #'<))))
