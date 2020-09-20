(defpackage :cp/test/mod-inverse
  (:use :cl :fiveam :cp/mod-inverse)
  (:import-from :cp/test/base #:base-suite))
(in-package :cp/test/mod-inverse)
(in-suite base-suite)

(test mod-inverse/random
  (let ((*num-trials* 1000))
    (for-all ((a (gen-integer :min 0 :max 1000))
              (m (gen-integer :min 2 :max 1000)))
      (is (or (/= 1 (gcd a m))
              (= 1 (mod (* a (mod-inverse a m)) m)))))))
