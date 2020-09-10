(defpackage :cp/test/2d-bit
  (:use :cl :fiveam :cp/2d-bit)
  (:import-from :cp/test/base #:base-suite))
(in-package :cp/test/2d-bit)
(in-suite base-suite)

(define-2d-bitree bitree
  :operator #'+
  :identity 0
  :sum-type fixnum)

(test 2d-bit
  (declare (notinline coerce-to-bitree! bitree-fold bitree-update!))
  (let ((tree (coerce-to-bitree! (make-array '(2 3) :initial-contents '((1 2 3) (4 5 6)))))
        (tree2 (make-array '(2 3) :initial-element 0)))
    (is (= 1 (bitree-fold tree 1 1)))
    (is (= 3 (bitree-fold tree 1 2)))
    (is (= 6 (bitree-fold tree 1 3)))
    (is (= 5 (bitree-fold tree 2 1)))
    (is (= 12 (bitree-fold tree 2 2)))
    (is (= 21 (bitree-fold tree 2 3)))
    (is (= 0
           (bitree-fold tree 0 3)
           (bitree-fold tree 0 2)
           (bitree-fold tree 0 1)
           (bitree-fold tree 0 0)))
    (dotimes (i 2)
      (dotimes (j 3)
        (bitree-update! tree2 i j (aref #2a((1 2 3) (4 5 6)) i j))))
    (is (equalp tree tree2))))
