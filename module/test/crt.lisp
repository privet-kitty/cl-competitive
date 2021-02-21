(defpackage :cp/test/crt
  (:use :cl :fiveam :cp/crt)
  (:import-from :cp/test/base #:base-suite))
(in-package :cp/test/crt)
(in-suite base-suite)

(test crt
  ;; Below are the sample cases on https://yukicoder.me/problems/447
  (is (= 10 (crt* #(10 10 10) #(20 30 40))))
  (is (= 70 (crt* #(10 10 30) #(20 30 40))))
  (is (equalp '(nil nil) (multiple-value-list (crt* #(1 0 5) #(2 4 17)))))
  (is (= 38774484298448350
         (crt* #(80712 320302 140367) #(221549 699312 496729))))
  ;; null case
  (is (equalp '(0 1) (multiple-value-list (crt* #() #())))))
