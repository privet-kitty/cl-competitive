(defpackage :cp/test/ratio-to-decimal
  (:use :cl :fiveam :cp/ratio-to-decimal)
  (:import-from :cp/test/base #:base-suite))
(in-package :cp/test/ratio-to-decimal)
(in-suite base-suite)

(test ratio-to-decimal
  (is (equalp #() (ratio-to-decimal 41/4 0)))
  (is (equalp #(10) (ratio-to-decimal 41/4 1)))
  (is (equalp #(10 2) (ratio-to-decimal 41/4 2)))
  (is (equalp #(10 2 5) (ratio-to-decimal 41/4 3)))
  (is (equalp #(10 2 5 0) (ratio-to-decimal 41/4 4)))
  (is (equalp #(0 3 3 3 3) (ratio-to-decimal 1/3 5))))
