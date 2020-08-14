(defpackage :cp/test/zeta-transform
  (:use :cl :fiveam :cp/zeta-transform)
  (:import-from :cp/test/base #:base-suite))
(in-package :cp/test/zeta-transform)
(in-suite base-suite)

(test zeta-transform
  (is (equalp #(1 1 1 1 1 1 1 1)
              (moebius-subtransform! (zeta-subtransform! (vector 1 1 1 1 1 1 1 1)))))
  (is (equalp #(1 2 2 4 2 4 4 8)
              (zeta-subtransform! (vector 1 1 1 1 1 1 1 1))))
  (is (equalp #(1 1 1 1 1 1 1 1)
              (moebius-supertransform! (zeta-supertransform! (vector 1 1 1 1 1 1 1 1)))))
  (is (equalp #(8 4 4 2 4 2 2 1)
              (zeta-supertransform! (vector 1 1 1 1 1 1 1 1)))))
