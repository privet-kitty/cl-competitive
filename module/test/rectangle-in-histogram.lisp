(defpackage :cp/test/rectangle-in-histogram
  (:use :cl :fiveam :cp/rectangle-in-histogram)
  (:import-from :cp/test/base #:base-suite))
(in-package :cp/test/rectangle-in-histogram)
(in-suite base-suite)

(test maxmize-rectangle-in-histogram
  (is (equal '(8 2 4)
             (multiple-value-list (maximize-rectangle-in-histogram #(2 1 4 5 1 3 3)))))
  (is (equal '(6 0 3)
             (multiple-value-list (maximize-rectangle-in-histogram #(2 2 2)))))
  (is (equal '(0 0 0)
             (multiple-value-list (maximize-rectangle-in-histogram #())))))
