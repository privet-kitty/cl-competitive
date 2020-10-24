(defpackage :cp/test/mod-binomial
  (:use :cl :fiveam :cp/mod-binomial :cp/binomial-coefficient-mod)
  (:import-from :cp/test/base #:base-suite))
(in-package :cp/test/mod-binomial)
(in-suite base-suite)

(test mod-binomial
  (finishes
    (loop for n from -20 to 20
          do (loop for k from -20 to 20
                   do (assert (= (mod-binomial n k +binom-mod+) (binom n k)))))))
