(eval-when (:compile-toplevel :load-toplevel :execute)
  (load "test-util")
  (load "../2d-rolling-hash.lisp"))

(use-package :test-util)

(declaim (notinline rhash2d-query rhash2d-matrix-hash))

(with-test (:name 2d-rolling-hash)
  (let ((rhash (make-rhash2d #2a((1 0 2 3)
                                 (1 3 1 4)
                                 (5 1 0 2)
                                 (0 1 3 1))
                             2 3)))
    (assert (= (rhash2d-query rhash 0 0)
               (rhash2d-query rhash 2 1)))
    (assert (/= (rhash2d-query rhash 0 0)
                (rhash2d-query rhash 0 1)))
    (assert (= (rhash2d-matrix-hash rhash #2a((0 2 3) (3 1 4)))
               (rhash2d-query rhash 0 1)))
    ;; zero case
    (make-rhash2d #2a() 0 0)))
