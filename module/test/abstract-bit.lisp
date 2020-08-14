(defpackage :cp/test/abstract-bit
  (:use :cl :fiveam :cp/abstract-bit)
  (:import-from :cp/test/base #:base-suite))
(in-package :cp/test/abstract-bit)
(in-suite base-suite)

(define-bitree bitree
  :operator #'+
  :identity 0
  :sum-type fixnum
  :order #'<)

(test abstract-bit
  (declare (notinline bitree-fold bitree-bisect-left bitree-bisect-right coerce-to-bitree!))
  (let ((tree (coerce-to-bitree! (vector 10 2 0 0 1 2 2)))
        (tree2 (coerce-to-bitree! (vector 1 0 0 1))))
    (is (= 0 (bitree-bisect-left tree -1)))
    (is (= 0 (bitree-bisect-left tree 0)))
    (is (= 0 (bitree-bisect-left tree 3)))
    (is (= 0 (bitree-bisect-left tree 10)))
    (is (= 1 (bitree-bisect-left tree 11)))
    (is (= 1 (bitree-bisect-left tree 12)))
    (is (= 4 (bitree-bisect-left tree 13)))
    (is (= 5 (bitree-bisect-left tree 14)))
    (is (= 6 (bitree-bisect-left tree 17)))
    (is (= 7 (bitree-bisect-left tree 18)))
    (is (= 7 (bitree-bisect-left tree 200)))
    (is (= 0 (bitree-bisect-left tree2 0)))
    (is (= 0 (bitree-bisect-left tree2 1)))
    (is (= 3 (bitree-bisect-left tree2 2)))
    (is (= 1 (bitree-fold tree2 3)))
    (is (= 2 (bitree-fold tree2 4)))
    (is (= 4 (bitree-bisect-left tree2 30000)))
    (is (= 0 (bitree-bisect-right tree2 -1)))
    (is (= 0 (bitree-bisect-right tree2 0)))
    (is (= 3 (bitree-bisect-right tree2 1)))
    (is (= 4 (bitree-bisect-right tree2 2)))
    (is (= 4 (bitree-bisect-right tree2 30000)))))
