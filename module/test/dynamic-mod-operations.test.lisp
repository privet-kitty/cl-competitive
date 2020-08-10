(eval-when (:compile-toplevel :load-toplevel :execute)
  (load "test-util")
  (load "../dynamic-mod-operations.lisp"))

(use-package :test-util)

(with-test (:name dynamic-mod-operations)
  (let ((*modulus* 7))
    (locally
        (declare (inline mod* mod+))
      (assert (= 3 (mod* 2 4 3)))
      (assert (= 6 (mod* -2 4)))
      (assert (= 2 (mod+ 2 4 3))))
    (locally
        (declare (notinline mod* mod+))
      (assert (= 3 (mod* 2 4 3)))
      (assert (= 6 (mod* -2 4)))
      (assert (= 2 (mod+ 2 4 3))))
    (let ((x 8))
      (assert (= 3 (progn (incfmod x 2) x)))
      (assert (= 6 (progn (decfmod x 4) x))))))
