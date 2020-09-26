(defpackage :cp/experimental/test/mod-inverse
  (:use :cl :fiveam :cp/experimental/mod-inverse)
  (:import-from :cp/test/base #:base-suite))
(in-package :cp/experimental/test/mod-inverse)
(in-suite base-suite)

(test experimental/mod-inverse/manual
  (loop for x from -10 to 10
        do (is (zerop (mod-inverse x 1)))))

(test experimental/mod-inverse/random
  (let ((*num-trials* 1000)
        (*test-dribble* nil))
    (macrolet
        ((frob (signal-p &rest dcls)
           `(locally (declare ,@dcls)
              (for-all ((a (gen-integer :min 0 :max 1000))
                        (m (gen-integer :min 2 :max 1000)))
                (if (/= 1 (gcd a m))
                    ,(if signal-p
                         `(signals division-by-zero (mod-inverse a m))
                         `(finishes (mod-inverse a m)))
                    (is (= 1 (mod (* a (mod-inverse a m)) m)))))
              (for-all ((a (gen-integer :min most-negative-fixnum :max most-positive-fixnum))
                        (m (gen-integer :min 2 :max most-positive-fixnum)))
                (if (/= 1 (gcd a m))
                    ,(if signal-p
                         `(signals division-by-zero (mod-inverse a m))
                         `(finishes (mod-inverse a m)))
                    (is (= 1 (mod (* a (mod-inverse a m)) m))))))))
      (frob t (inline mod-inverse))
      (frob nil (inline mod-inverse) (optimize (safety 0)))
      (frob t (notinline mod-inverse))
      (frob t (notinline mod-inverse) (optimize (safety 0))))))
