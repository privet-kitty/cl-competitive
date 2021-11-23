(defpackage :cp/test/montmort
  (:use :cl :fiveam :cp/montmort)
  (:import-from :cp/test/base #:base-suite))
(in-package :cp/test/montmort)
(in-suite base-suite)

(test montmort/hand
  (is (equalp #() (make-montmort-sequence 0 1)))
  (is (equalp #() (make-montmort-sequence 0 10)))
  (is (equalp #(1 0 1 2 9 44 265 1854 14833 133496)
              (make-montmort-sequence 10 998244353)))
  (is (equalp #(0 0 0 0 0 0 0 0 0 0)
              (make-montmort-sequence 10 1)))
  (is (equal '(unsigned-byte 31) (array-element-type (make-montmort-sequence 0 10))))
  (is (equal 'fixnum (array-element-type
                      (make-montmort-sequence 0 10 'fixnum)))))
