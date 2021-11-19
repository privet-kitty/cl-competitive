(defpackage :cp/test/mod-sqrt
  (:use :cl :fiveam :cp/mod-sqrt)
  (:import-from :cp/test/base #:base-suite))
(in-package :cp/test/mod-sqrt)
(in-suite base-suite)

(test mod-sqrt/hand
  (dolist (p '(2 3 5 7 11 13 17 1000000007))
    (is (= 0 (mod-sqrt 0 p)))
    (is (= 1 (mod-sqrt 1 p))))
  (is (null (mod-sqrt 2 3)))
  (is (null (mod-sqrt 2 5)))
  (is (null (mod-sqrt 3 5))))

(test mod-sqrt/random
  (let ((*random-state* (sb-ext:seed-random-state 0))
        (*test-dribble* nil))
    (dolist (p '(3 5 7 11 13 10007 1000000007))
      (dotimes (_ 1000)
        (let* ((a (random most-positive-fixnum))
               (sqrt (mod-sqrt a p)))
          (when sqrt
            (is (= (mod (* sqrt sqrt) p) (mod a p)))))))))
