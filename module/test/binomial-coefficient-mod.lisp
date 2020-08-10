(defpackage :cp/test/binomial-coefficient-mod
  (:use :cl :fiveam :cp/binomial-coefficient-mod)
  (:import-from :cp/test/base #:base-suite))
(in-package :cp/test/binomial-coefficient-mod)
(in-suite base-suite)

(test binom
  (is (= 0 (binom -1 1)))
  (is (= 0 (binom 1 -2)))
  (is (= 0 (binom 1 3)))
  (is (= 1 (binom 0 0)))
  (is (= 10 (binom 5 2)))
  (is (= 962556519 (binom 1000 400))))

(test catalan
  (is (= 1 (catalan 0)))
  (is (= 1 (catalan 1)))
  (is (= 429 (catalan 7)))
  (is (= (mod 6564120420 +binom-mod+)
         (catalan 20))))

(test stirling2
  (let ((mat #2a((1 0 0 0 0 0 0 0)
                 (0 1 0 0 0 0 0 0)
                 (0 1 1 0 0 0 0 0)
                 (0 1 3 1 0 0 0 0)
                 (0 1 7 6 1 0 0 0)
                 (0 1 15 25 10 1 0 0)
                 (0 1 31 90 65 15 1 0))))
    (finishes (dotimes (n 7)
                (dotimes (k 8)
                  (assert (= (aref mat n k) (stirling2 n k))))))
    (is (= (stirling2 15 6)
           (mod 420693273 +binom-mod+)))))
