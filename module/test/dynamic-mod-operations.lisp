(defpackage :cp/test/dynamic-mod-operations
  (:use :cl :fiveam :cp/mod-operations)
  (:import-from :cp/test/base #:base-suite))
(in-package :cp/test/dynamic-mod-operations)
(in-suite base-suite)

(define-mod-operations *modulus*)

(test dynamic-mod-operations
  (let ((*modulus* 7))
    (locally
        (declare (inline mod* mod+))
      (is (= 1 (mod*)))
      (is (= 0 (mod+)))
      (is (= 6 (mod- 1)))
      (is (= 3 (mod* 2 4 3)))
      (is (= 6 (mod* -2 4)))
      (is (= 2 (mod+ 2 4 3))))
    (locally
        (declare (notinline mod* mod+))
      (is (= 1 (mod*)))
      (is (= 0 (mod+)))
      (is (= 6 (mod- 1)))
      (is (= 3 (mod* 2 4 3)))
      (is (= 6 (mod* -2 4)))
      (is (= 2 (mod+ 2 4 3))))
    (let ((x 8))
      (is (= 3 (progn (incfmod x 2) x)))
      (is (= 6 (progn (decfmod x 4) x)))))
  (let ((*modulus* 1))
    (locally
        (declare (inline mod* mod+))
      (is (zerop (mod*)))
      (is (zerop (mod+)))
      (is (zerop (mod- 1)))
      (is (zerop (mod* -2 4)))
      (is (zerop (mod+ 2 4 3)))
      (is (zerop (mod- 2 4 3))))
    (locally
        (declare (notinline mod* mod+))
      (is (zerop (mod*)))
      (is (zerop (mod+)))
      (is (zerop (mod- 1)))
      (is (zerop (mod* -2 4)))
      (is (zerop (mod+ 2 4 3)))
      (is (zerop (mod- 2 4 3))))
    (let ((x 8))
      (is (zerop (progn (incfmod x 2) x)))
      (is (zerop (progn (decfmod x 4) x))))))
