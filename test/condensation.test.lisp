(eval-when (:compile-toplevel :load-toplevel :execute)
  (load "test-util")
  (load "../condensation.lisp"))

(use-package :test-util)

(with-test (:name scc)
  ;; null graph
  (let ((scc (make-scc #())))
    (assert (equalp #() (scc-components scc))))
  ;; graph of a vertex
  (let ((scc (make-scc #(()))))
    (assert (equalp #(0) (scc-components scc))))
  ;; graph of two vertices
  (let ((scc (make-scc #((1) ()))))
    (assert (or (equalp #(0 1) (scc-components scc))
                (equalp #(1 0) (scc-components scc)))))
  (let ((scc (make-scc #(() (0)))))
    (assert (or (equalp #(0 1) (scc-components scc))
                (equalp #(1 0) (scc-components scc)))))
  (let ((scc (make-scc #((1) (0)))))
    (assert (equalp #(0 0) (scc-components scc))))
  (let ((scc (make-scc #(() ()))))
    (assert (or (equalp #(0 1) (scc-components scc))
                (equalp #(1 0) (scc-components scc)))))
  
  (let ((scc (make-scc #((1) (2) (0)))))
    (assert (equalp #(0 0 0) (scc-components scc))))
  (let ((scc (make-scc #((1) (0) ()))))
    (assert (or (equalp #(0 0 1) (scc-components scc))
                (equalp #(1 1 0) (scc-components scc)))))
  
  (let ((scc (make-scc #((1) (2) (0)) #((2) (0) (1)))))
    (assert (equalp #(0 0 0) (scc-components scc)))))
