(eval-when (:compile-toplevel :load-toplevel :execute)
  (load "test-util")
  (load "../symmetric-group.lisp")
  (load "set-equal.lisp"))

(use-package :test-util)

(with-test (:name cyclic-permutation)
  (assert (set-equal '((5) (2) (1 4 6 8 3 7) (0))
                     (decompose-to-cycles #(0 4 2 7 6 5 8 1 3))
                     :test #'equal))
  (assert (equal '((0)) (decompose-to-cycles #(0))))
  (assert (equal nil (decompose-to-cycles #()))))
