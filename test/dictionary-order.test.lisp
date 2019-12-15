(eval-when (:compile-toplevel :load-toplevel :execute)
  (load "test-util")
  (load "../dictionary-order.lisp"))

(use-package :test-util)

(with-test (:name dictionary-order)
  ;; list
  (assert (dict< #'< '(1 1) '(1 2)))
  (assert (dict< #'< '(1 1) '(1 1 0)))
  (assert (not (dict< #'< '(1 2) '(1 2))))
  (assert (not (dict< #'< '(1 3) '(1 2))))
  (assert (dict< #'< '() '(1)))
  (assert (not (dict< #'< '() '())))
  ;; vector
  (assert (dict< #'< #(1 1) #(1 2)))
  (assert (dict< #'< #(1 1) #(1 1 0)))
  (assert (not (dict< #'< #(1 2) #(1 2))))
  (assert (not (dict< #'< #(1 3) #(1 2))))
  (assert (dict< #'< #() #(1)))
  (assert (not (dict< #'< #() #()))))
