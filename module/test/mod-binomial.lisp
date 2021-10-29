(defpackage :cp/test/mod-binomial
  (:use :cl :fiveam :cp/mod-binomial :cp/binom-mod-prime :cp/static-mod)
  (:import-from :cp/test/base #:base-suite))
(in-package :cp/test/mod-binomial)
(in-suite base-suite)

(test mod-binomial
  (let ((*test-dribble* nil))
    (loop for n from -20 to 20
          do (loop for k from -20 to 20
                   do (is (= (mod-binomial n k +mod+)
                             (binom n k)))
                      (is (= (mod-multichoose n k +mod+)
                             (multichoose n k)))))))
