(eval-when (:compile-toplevel :load-toplevel :execute)
  (load "test-util")
  (load "../gray-code.lisp"))

(use-package :test-util)

(with-test (:name gray-code)
  (dotimes (x 100)
    (assert (= (natural-to-gray (gray-to-natural x))))
    (assert (= (gray-to-natural (natural-to-gray x))))))
