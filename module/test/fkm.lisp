(defpackage :cp/test/fkm
  (:use :cl :fiveam :cp/fkm :cp/test/set-equal)
  (:import-from :cp/test/base #:base-suite))
(in-package :cp/test/fkm)
(in-suite base-suite)

(defun collect-necklaces (length alphabet-size)
  (let (res)
    (map-necklaces
     (lambda (x) (push (copy-seq x) res))
     length
     alphabet-size)
    res))

(test fkm
  (is (set-equal (collect-necklaces 4 2)
                 '(#(0 0 0 0)
                   #(0 0 0 1)
                   #(0 0 1 1)
                   #(0 1 0 1)
                   #(0 1 1 1)
                   #(1 1 1 1))
                 :test #'equalp))
  (is (set-equal (collect-necklaces 5 1)
                 '(#(0 0 0 0 0))
                 :test #'equalp))
  (is (set-equal (collect-necklaces 1 5)
                 '(#(0) #(1) #(2) #(3) #(4))
                 :test #'equalp))
  (is (set-equal (collect-necklaces 0 4)
                 '(#())
                 :test #'equalp)))
