(eval-when (:compile-toplevel :load-toplevel :execute)
  (load "test-util")
  (load "../chinese-remainder.lisp"))

(use-package :test-util)

(with-test (:name chinese-rem)
  (assert (= 10 (chinese-rem #(10 10 10) #(20 30 40))))
  (assert (= 70 (chinese-rem #(10 10 30) #(20 30 40))))
  (assert (equalp '(nil nil) (multiple-value-list (chinese-rem #(1 0 5) #(2 4 17)))))
  (assert (= 38774484298448350
             (chinese-rem #(80712 320302 140367) #(221549 699312 496729))))
  ;; null case
  (assert (equalp '(0 1) (multiple-value-list (chinese-rem #() #())))))
