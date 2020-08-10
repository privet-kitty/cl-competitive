(defpackage :cp/test/dictionary-order
  (:use :cl :fiveam :cp/dictionary-order)
  (:import-from :cp/test/base #:base-suite))
(in-package :cp/test/dictionary-order)
(in-suite base-suite)

(test dictionary-order
  ;; list
  (is (dict< #'< '(1 1) '(1 2)))
  (is (dict< #'< '(1 1) '(1 1 0)))
  (is (not (dict< #'< '(1 2) '(1 2))))
  (is (not (dict< #'< '(1 3) '(1 2))))
  (is (dict< #'< '() '(1)))
  (is (not (dict< #'< '() '())))
  ;; vector
  (is (dict< #'< #(1 1) #(1 2)))
  (is (dict< #'< #(1 1) #(1 1 0)))
  (is (not (dict< #'< #(1 2) #(1 2))))
  (is (not (dict< #'< #(1 3) #(1 2))))
  (is (dict< #'< #() #(1)))
  (is (not (dict< #'< #() #()))))
