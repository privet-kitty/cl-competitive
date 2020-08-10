(eval-when (:compile-toplevel :load-toplevel :execute)
  (load "test-util")
  (load "../gray-code.lisp"))

(use-package :test-util)

(with-test (:name gray-code)
  (assert (zerop (gray-to-natural 0)))
  (assert (zerop (natural-to-gray 0)))
  (dotimes (x 100)
    (assert (= (natural-to-gray (gray-to-natural x))))
    (assert (= (gray-to-natural (natural-to-gray x))))
    (assert (= 1 (logcount (logxor (natural-to-gray (+ x 1)) (natural-to-gray x)))))))
