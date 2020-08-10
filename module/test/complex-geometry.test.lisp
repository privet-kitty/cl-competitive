(eval-when (:compile-toplevel :load-toplevel :execute)
  (load "test-util")
  (load "../complex-geometry.lisp"))

(use-package :test-util)

(with-test (:name cross-product)
  (assert (= 0 (cross-product #c(1 3) #c(2 6))))
  (assert (= -6 (cross-product #c(5 7) #c(13 17)))))

(with-test (:name calc-circumcenter)
  (assert (null (calc-circumcenter #c(0 0) #c(1 1) #c(2 2))))
  (assert (not (null (calc-circumcenter #c(0 0) #c(1 0) #c(1 1))))))
