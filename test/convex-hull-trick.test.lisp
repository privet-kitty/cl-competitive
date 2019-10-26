(eval-when (:compile-toplevel :load-toplevel :execute)
  (load "test-util")
  (load "../convex-hull-trick.lisp"))

(use-package :test-util)

(with-test (:name convex-hull-trick)
  ;; Examples by satanic0258
  ;; https://satanic0258.hatenablog.com/entry/2016/08/16/181331
  (let ((cht (make-cht 10)))
    (cht-push cht 2 2)
    (cht-push cht 1 3)
    (cht-push cht -1 10)
    (cht-push cht -2 15)
    (assert (= 0 (cht-get cht -1)))
    (assert (= 2 (cht-get cht 0)))
    (assert (= 4 (cht-get cht 1)))
    (assert (= 5 (cht-get cht 2)))
    (assert (= 6 (cht-get cht 3)))
    (assert (= 6 (cht-get cht 4)))
    (assert (= 1 (cht-get cht 7))))

  (let ((cht (make-cht 10)))
    (cht-push cht 2 0)
    (assert (= -4 (cht-get cht -2)))
    (assert (= 4 (cht-get cht 2)))
    (cht-push cht 1 1)
    (cht-push cht 0 -1)
    (assert (= -4 (cht-get cht -2)))
    (assert (= -1 (cht-get cht 2)))
    (cht-push cht -1 0)
    (assert (= 3 (%cht-length cht)))
    (assert (= -4 (cht-get cht -2)))
    (assert (= -2 (cht-get cht 2)))))