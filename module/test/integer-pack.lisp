(defpackage :cp/test/integer-pack
  (:use :cl :fiveam :cp/integer-pack)
  (:import-from :cp/test/base #:base-suite))
(in-package :cp/test/integer-pack)
(in-suite base-suite)

(define-integer-pack intnode (slot1 3) (slot2 10) (slot3 1))
(test define-integer-pack
  (let ((node (pack-intnode 3 2 1)))
    (is (= 3 (intnode-slot1 node)))
    (is (= 2 (intnode-slot2 node)))
    (is (= 1 (intnode-slot3 node)))
    (with-unpacking-intnode (s1 s2 s3) node
      (is (= 3 s1))
      (is (= 2 s2))
      (is (= 1 s3)))))
