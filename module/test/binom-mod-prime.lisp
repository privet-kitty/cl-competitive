(defpackage :cp/test/binom-mod-prime
  (:use :cl :fiveam :cp/binom-mod-prime :cp/mod-binomial :cp/static-mod)
  (:import-from :cp/test/base #:base-suite))
(in-package :cp/test/binom-mod-prime)
(in-suite base-suite)

(test binom
  (is (= 0 (binom -1 1)))
  (is (= 0 (binom 1 -2)))
  (is (= 0 (binom 1 3)))
  (is (= 1 (binom 0 0)))
  (is (= 10 (binom 5 2)))
  (is (= (mod-binomial 1000 400 +mod+) (binom 1000 400))))

(test catalan
  (is (= 1 (catalan 0)))
  (is (= 1 (catalan 1)))
  (is (= 429 (catalan 7)))
  (is (= (mod 6564120420 +mod+)
         (catalan 20))))

(test multichoose
  (is (= 6 (multichoose 3 2)))
  (is (= 1 (multichoose 1 1)))
  (is (= 1 (multichoose 1 0)))
  (is (= 0 (multichoose 0 1)))
  (is (= 0 (multichoose 0 0))))

(test multinomial
  (is (= 1 (multinomial)))
  (is (= 1 (multinomial 0)))
  (is (= 1 (multinomial 3)))
  (is (= 1 (multinomial 3 0)))
  (is (= (binom 8 3) (multinomial 3 5)))
  (is (= 60 (multinomial 1 2 3))))

(test stirling2
  (let ((mat #2a((1 0 0 0 0 0 0 0)
                 (0 1 0 0 0 0 0 0)
                 (0 1 1 0 0 0 0 0)
                 (0 1 3 1 0 0 0 0)
                 (0 1 7 6 1 0 0 0)
                 (0 1 15 25 10 1 0 0)
                 (0 1 31 90 65 15 1 0))))
    (dotimes (n 7)
      (dotimes (k 8)
        (is (= (aref mat n k) (stirling2 n k)))))
    (is (= (stirling2 15 6)
           (mod 420693273 +mod+)))))
