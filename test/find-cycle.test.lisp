(eval-when (:compile-toplevel :load-toplevel :execute)
  (load "test-util")
  (load "../find-cycle.lisp"))

(use-package :test-util)

(with-test (:name relative-error)
  ;; cycle of length 2
  (let ((graph #((1) (0 2) (1 3) (2 1))))
    (assert (equal '(1 2 3 1) (find-cycle graph :wrap t :undirected t)))
    (assert (equal '(1 2 3) (find-cycle graph :undirected t)))
    (assert (equal '(0 1 0) (find-cycle #((1) (0 2) (1 3) (2 1)) :wrap t)))
    (assert (equal '(0 1) (find-cycle #((1) (0 2) (1 3) (2 1))))))
  ;; self-loop
  (let ((graph #((2) () (2))))
    (assert (equal '(2) (find-cycle graph)))
    (assert (equal '(2 2) (find-cycle graph :wrap t)))
    (assert (equal '(2) (find-cycle graph :undirected t)))
    (assert (equal '(2 2) (find-cycle graph :undirected t :wrap t))))
  ;; forest
  (let ((graph #(() (2) (1 3) (2 4) (3 2))))
    (assert (equal '(2 3 4) (find-cycle graph :undirected t)))
    (assert (equal '(2 3 4 2) (find-cycle graph :wrap t :undirected t)))))
