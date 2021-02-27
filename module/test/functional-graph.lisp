(defpackage :cp/test/functional-graph
  (:use :cl :fiveam :cp/functional-graph)
  (:import-from :cp/test/base #:base-suite))
(in-package :cp/test/functional-graph)
(in-suite base-suite)

(test make-cycle-info
  (multiple-value-bind (cycle lengths)
      (make-cycle-info #(1 2 3 4 5 6 3 8 1 10 11 10 6 12 15 14))
    (is (equalp #(3 3 3 3 4 5 6 3 3 10 10 11 6 6 14 15) cycle))
    (is (equalp #(3 2 1 4 4 4 4 4 3 1 2 2 1 2 2 2) lengths)))
  ;; self-loop
  (multiple-value-bind (cycle lengths) (make-cycle-info #(1 2 2))
    (is (equalp #(2 2 2) cycle))
    (is (equalp #(2 1 1) lengths)))
  ;; empty graph
  (multiple-value-bind (cycle lengths) (make-cycle-info #())
    (is (equalp #() cycle))
    (is (equalp #() lengths))))
