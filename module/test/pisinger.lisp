(defpackage :cp/test/pisinger
  (:use :cl :fiveam :cp/pisinger)
  (:import-from :cp/test/base #:base-suite))
(in-package :cp/test/pisinger)
(in-suite base-suite)

(test subset-sum/zero
  (declare (notinline subset-sum))
  (is (equal #* (subset-sum #() 0)))
  (is (eql t (subset-sum #() 0 nil)))
  (is (typep (subset-sum #(0) 0) 'simple-bit-vector))
  (is (eql t (subset-sum #(0) 0 nil)))
  (is (typep (subset-sum #(3 0) 0) 'simple-bit-vector))
  (is (eql t (subset-sum #(3 0) 0 nil)))
  (is (equalp #*00 (subset-sum #(3 3) 0)))
  (is (eql t (subset-sum #(3 3) 0 nil)))
  (is (null (subset-sum #(0 0) 3)))
  (is (null (subset-sum #(0 0) 3 nil)))
  (is (null (subset-sum #() 3)))
  (is (null (subset-sum #() 3 nil))))

(test subset-sum/hand
  (is (equalp #*111 (subset-sum #(1 2 3) 6)))
  (is (equalp #*101 (subset-sum #(1 2 3) 4)))
  (is (equalp #*011 (subset-sum #(1 2 3) 5)))
  (is (equalp #*100 (subset-sum #(1 2 3) 1)))
  (is (equalp #*000 (subset-sum #(1 2 3) 0)))
  (is (null (subset-sum #(1 2 3) 7))))
