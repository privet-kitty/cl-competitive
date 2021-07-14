(defpackage :cp/test/mod-inverse
  (:use :cl :fiveam :cp/mod-inverse)
  (:import-from :cp/test/base #:base-suite)
  (:import-from :sb-kernel #:%simple-fun-type))
(in-package :cp/test/mod-inverse)
(in-suite base-suite)

(defun test1 (x y)
  (declare (integer x)
           ((integer -3 5) y))
  (mod-inverse x y))

(defun test2 (x y)
  (declare (integer x)
           ((integer -3 *) y))
  (mod-inverse x y))

(test mod-inverse/type
  (is (equalp (%simple-fun-type #'test1)
              '(function (integer (integer -3 5)) (values (mod 5) &optional))))
  (is (equalp (%simple-fun-type #'test2)
              '(function (integer (integer -3)) (values unsigned-byte &optional)))))

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
