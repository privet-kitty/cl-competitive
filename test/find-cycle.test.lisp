(eval-when (:compile-toplevel :load-toplevel :execute)
  (load "test-util")
  (load "../find-cycle.lisp"))

(use-package :test-util)

(with-test (:name relative-error)
  (assert (equal '(1 2 3) (find-cycle #((1) (0 2) (1 3) (2 1)))))
  ;; exclude self-loops
  (assert (equal '(1 2 3) (find-cycle #((1 0) (0 2) (1 3) (2 1)))))
  (assert (null (find-cycle #((0) () (0)))))
  ;; forest
  (assert (equal '(2 3 4) (find-cycle #(() (2) (1 3) (2 4) (3 2)))))
  ;; wrap
  (assert (equal '(2 3 4) (find-cycle #(() (2) (1 3) (2 4) (3 2))))))
