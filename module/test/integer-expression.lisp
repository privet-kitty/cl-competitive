(defpackage :cp/test/integer-expression
  (:use :cl :fiveam :cp/integer-expression :cp/integer-length)
  (:import-from :cp/test/base #:base-suite))
(in-package :cp/test/integer-expression)
(in-suite base-suite)

(test integer-reverse*/hand
  (is (= 1234560987 (integer-reverse* 1234567890 0 4)))
  (is (= 1234567890 (integer-reverse* 1234567890 0 1)))
  (is (= 1234587690 (integer-reverse* 1234567890 2 5)))
  (is (= 1234567890 (integer-reverse* 1234567890 7 7)))
  (is (= 21034567890 (integer-reverse* 1234567890 8 11)))
  (is (= #x1234587690 (integer-reverse* #x1234567890 2 5 16)))
  (is (= #x321004567890 (integer-reverse* #x1234567890 7 12 16)))
  (is (= #x-1234587690 (integer-reverse* #x-1234567890 2 5 16)))
  (is (= -1230 (integer-reverse* -1320 1 3)))
  (is (zerop (integer-reverse* 0 4 30 123))))

(test integer-reverse/random
  (let ((*test-dribble* nil)
        (state (sb-ext:seed-random-state 0)))
    (dotimes (_ 1000)
      (let ((x (- (random most-positive-fixnum state)
                  (floor most-positive-fixnum 2)))
            (base (+ 2 (random 20 state)))
            (l (random 30 state))
            (r (random 30 state)))
        (when (> l r)
          (rotatef l r))
        (is (= x (integer-reverse* (integer-reverse* x l r base) l r base)))
        (is (= (integer-reverse x base)
               (integer-reverse* x 0 (integer-length* x base) base)))))))

(test integer-reverse
  (loop for base from 2 to 10
        do (is (zerop (integer-reverse 0))))
  (is (= #x321 (integer-reverse #x1230 16))))
