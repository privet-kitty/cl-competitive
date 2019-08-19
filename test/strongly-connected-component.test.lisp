(eval-when (:compile-toplevel :load-toplevel :execute)
  (load "test-util")
  (load "../strongly-connected-component.lisp"))

(use-package :test-util)

(with-test (:name scc)
  ;; graph of two vertices
  (let ((scc (make-scc #((1) ()))))
    (assert (or (equalp #(0 1) (scc-result scc))
                (equalp #(1 0) (scc-result scc)))))
  (let ((scc (make-scc #(() (0)))))
    (assert (or (equalp #(0 1) (scc-result scc))
                (equalp #(1 0) (scc-result scc)))))
  (let ((scc (make-scc #((1) (0)))))
    (assert (equalp #(0 0) (scc-result scc))))
  (let ((scc (make-scc #(() ()))))
    (assert (or (equalp #(0 1) (scc-result scc))
                (equalp #(1 0) (scc-result scc))))))
