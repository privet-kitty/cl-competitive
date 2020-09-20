(defpackage :cp/test/ssp
  (:use :cl :fiveam :cp/ssp :cp/min-cost-flow)
  (:import-from :cp/test/base #:base-suite))
(in-package :cp/test/ssp)
(in-suite base-suite)

(defun make-graph ()
  (let ((graph (make-array '(5) :element-type 'list :initial-element nil)))
    (add-cedge graph 0 1 2 10)
    (add-cedge graph 0 2 4 2)
    (add-cedge graph 1 2 6 6)
    (add-cedge graph 1 3 2 6)
    (add-cedge graph 3 2 3 3)
    (add-cedge graph 3 4 6 8)
    (add-cedge graph 2 4 2 5)
    graph))

(test ssp
  (let ((graph (make-graph)))
    (is (= 80 (min-cost-flow! graph 0 4 9)))
    (is (= 0 (min-cost-flow! graph 0 4 0))))
  (let ((graph (make-graph)))
    (is (= 102 (min-cost-flow! graph 0 4 11))))
  (let ((graph (make-graph)))
    (signals not-enough-capacity-error (min-cost-flow! graph 0 4 90)))
  (let ((graph (make-graph)))
    (is (= 102 (min-cost-flow! graph 0 4 11 :if-overflow nil))))

  (let ((graph (make-graph)))
    (is (= 80 (min-cost-flow! graph 0 4 9 :bellman-ford t)))
    (is (= 0 (min-cost-flow! graph 0 4 0 :bellman-ford t))))
  (let ((graph (make-graph)))
    (is (= 102 (min-cost-flow! graph 0 4 11 :bellman-ford t))))
  (let ((graph (make-graph)))
    (signals not-enough-capacity-error (min-cost-flow! graph 0 4 90 :bellman-ford t)))
  (let ((graph (make-graph)))
    (is (= 102 (min-cost-flow! graph 0 4 11 :if-overflow nil :bellman-ford t)))))
