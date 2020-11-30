(defpackage :cp/test/tree-path
  (:use :cl :fiveam :cp/tree-path)
  (:import-from :cp/test/base #:base-suite))
(in-package :cp/test/tree-path)
(in-suite base-suite)

(test tree-render-path
  (let ((graph #((1 2) (0 3) (0 4 5) (1) (2 6 7) (2) (4) (4))))
    (is (equal '(0 1 3) (tree-render-path graph 0 3)))
    (is (equal '(0) (tree-render-path graph 0 0)))
    (is (equal '(6 4 7) (tree-render-path graph 6 7)))
    (is (equal '(3 1 0 2 4) (tree-render-path graph 3 4)))
    (is (equal '(3 1 0 2 4 7) (tree-render-path graph 3 7)))))
