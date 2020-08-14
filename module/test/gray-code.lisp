(defpackage :cp/test/gray-code
  (:use :cl :fiveam :cp/gray-code)
  (:import-from :cp/test/base #:base-suite))
(in-package :cp/test/gray-code)
(in-suite base-suite)

(test gray-code
  (is (zerop (gray-to-natural 0)))
  (is (zerop (natural-to-gray 0)))
  (finishes
    (dotimes (x 100)
      (assert (= (natural-to-gray (gray-to-natural x))))
      (assert (= (gray-to-natural (natural-to-gray x))))
      (assert (= 1 (logcount (logxor (natural-to-gray (+ x 1)) (natural-to-gray x))))))))
