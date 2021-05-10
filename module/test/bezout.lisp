(defpackage :cp/test/bezout
  (:use :cl :fiveam :cp/bezout)
  (:import-from :cp/test/base #:base-suite))
(in-package :cp/test/bezout)
(in-suite base-suite)

(test solve-bezout
  (is (= (%calc-min-factor 8 3) -2))
  (is (= (%calc-min-factor -8 3) 3))
  (is (= (%calc-min-factor 8 -3) 2))
  (is (= (%calc-min-factor -8 -3) -3))
  (is (= (%calc-max-factor 8 3) -3))
  (is (= (%calc-max-factor -8 3) 2))
  (is (= (%calc-max-factor 8 -3) 3))
  (is (= (%calc-max-factor -8 -3) -2)))

(test count-bezout/random
  (let ((*test-dribble* nil)
        (*random-state* (sb-ext:seed-random-state 0)))
    (dotimes (_ 10000)
      (let ((a (- (random 40) 20))
            (b (- (random 40) 20))
            (c (- (random 40) 20))
            (max (random 100)))
        (let ((res (if (zerop b)
                       (loop for x from 1 to max
                             count (zerop (- c (* a x))))
                       (loop for x from 1 to max
                             count (zerop (mod (- c (* a x)) b))))))
          (is (= res (count-bezout a b c max)))
          (when (if (zerop b)
                    (zerop c)
                    (zerop (mod c b)))
            (incf res))
          (is (= res (count-bezout a b c max t))))))))
