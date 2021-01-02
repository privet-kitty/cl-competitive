(defpackage :cp/test/chromatic-number
  (:use :cl :fiveam :cp/chromatic-number)
  (:import-from :cp/test/base #:base-suite))
(in-package :cp/test/chromatic-number)
(in-suite base-suite)

(test calc-chromatic-number
  (is (= 1 (calc-chromatic-number #2a((0 0 0) (0 0 0) (0 0 0)))))
  (is (= 3 (calc-chromatic-number #2a((0 1 1) (1 0 1) (1 1 0)))))
  (is (= 2 (calc-chromatic-number #2a((0 1 0 1)
                                      (1 0 1 0)
                                      (0 1 0 1)
                                      (1 0 1 0))))))

;; TODO: random test using O(n^3) DP
