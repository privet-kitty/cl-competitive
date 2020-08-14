(defpackage :cp/test/mod-operations
  (:use :cl :fiveam :cp/mod-operations)
  (:import-from :cp/test/base #:base-suite))
(in-package :cp/test/mod-operations)
(in-suite base-suite)

(define-mod-operations 7)

(test mod-operations
  (locally
      (declare (inline mod* mod+))
    (is (= 3 (mod* 2 4 3)))
    (is (= 6 (mod* -2 4)))
    (is (= 2 (mod+ 2 4 3))))
  (locally
      (declare (notinline mod* mod+))
    (is (= 3 (mod* 2 4 3)))
    (is (= 6 (mod* -2 4)))
    (is (= 2 (mod+ 2 4 3))))
  (let ((x 8))
    (is (= 3 (progn (incfmod x 2) x)))
    (is (= 6 (progn (decfmod x 4) x)))))
