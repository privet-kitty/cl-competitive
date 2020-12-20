(defpackage :cp/test/mod-inverse
  (:use :cl :fiveam :cp/mod-inverse)
  (:import-from :cp/test/base #:base-suite))
(in-package :cp/test/mod-inverse)
(in-suite base-suite)

(test mod-inverse/hand
  (loop for x from -10 to 10
        do (is (zerop (mod-inverse x 1)))))

(test mod-inverse/random
  (let ((*num-trials* 1000)
        (*test-dribble* nil))
    (macrolet
        ((frob (&rest dcls)
           `(locally (declare ,@dcls)
              (for-all ((a (gen-integer :min 0 :max 1000))
                        (m (gen-integer :min 2 :max 1000)))
                (if (/= 1 (gcd a m))
                    (signals division-by-zero (mod-inverse a m))
                    (is (= 1 (mod (* a (mod-inverse a m)) m)))))
              (for-all ((a (gen-integer :min most-negative-fixnum :max most-positive-fixnum))
                        (m (gen-integer :min 2 :max most-positive-fixnum)))
                (if (/= 1 (gcd a m))
                    (signals division-by-zero (mod-inverse a m))
                    (is (= 1 (mod (* a (mod-inverse a m)) m))))))))
      (frob (inline mod-inverse))
      (frob (notinline mod-inverse)))))
