(defpackage :cp/test/xor
  (:use :cl :fiveam :cp/xor)
  (:import-from :cp/test/base #:base-suite))
(in-package :cp/test/xor)
(in-suite base-suite)

(test xor
  (is (null (xor)))
  (is (null (xor nil)))
  (is (= 3 (xor 3)))
  (is (= 1 (xor 1 nil)))
  (is (= 2 (xor nil 2)))
  (is (null (xor nil nil)))
  (is (null (xor 1 2)))
  (is (null (xor 1 2 nil)))
  (signals error (xor 1 2 (error "Huh?")))
  (is (null (xor 1 nil 2)))
  (is (= 2 (xor nil nil 2))))
