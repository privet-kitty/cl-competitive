(defpackage :cp/test/ssp-slow
  (:use :cl :fiveam :cp/ssp-slow :cp/min-cost-flow)
  (:import-from :cp/test/base #:base-suite))
(in-package :cp/test/ssp-slow)
(in-suite base-suite)

(test ssp-slow
  (let ((graph (make-array '(5) :element-type 'list :initial-element nil)))
    (add-cedge graph 0 1 2 10)
    (add-cedge graph 0 2 4 2)
    (add-cedge graph 1 2 6 6)
    (add-cedge graph 1 3 2 6)
    (add-cedge graph 3 2 3 3)
    (add-cedge graph 3 4 6 8)
    (add-cedge graph 2 4 2 5)
    (assert (= 80 (min-cost-flow! graph 0 4 9)))
    (assert (= 0 (min-cost-flow! graph 0 4 0)))
    (signals not-enough-capacity-error (min-cost-flow! graph 0 4 90))))
