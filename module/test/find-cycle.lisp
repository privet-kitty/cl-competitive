(defpackage :cp/test/find-cycle
  (:use :cl :fiveam :cp/find-cycle)
  (:import-from :cp/test/base #:base-suite))
(in-package :cp/test/find-cycle)
(in-suite base-suite)

(test find-cycle
  ;; cycle of length 2
  (let ((graph #((1) (0 2) (1 3) (2 1))))
    (is (equal '(1 2 3 1) (find-cycle graph :wrap t :undirected t)))
    (is (equal '(1 2 3) (find-cycle graph :undirected t)))
    (is (equal '(0 1 0) (find-cycle #((1) (0 2) (1 3) (2 1)) :wrap t)))
    (is (equal '(0 1) (find-cycle #((1) (0 2) (1 3) (2 1))))))
  ;; self-loop
  (let ((graph #((2) () (2))))
    (is (equal '(2) (find-cycle graph)))
    (is (equal '(2 2) (find-cycle graph :wrap t)))
    (is (equal '(2) (find-cycle graph :undirected t)))
    (is (equal '(2 2) (find-cycle graph :undirected t :wrap t))))
  ;; forest
  (let ((graph #(() (2) (1 3) (2 4) (3 2))))
    (is (equal '(2 3 4) (find-cycle graph :undirected t)))
    (is (equal '(2 3 4 2) (find-cycle graph :wrap t :undirected t)))))
