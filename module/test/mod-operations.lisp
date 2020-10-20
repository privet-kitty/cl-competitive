(defpackage :cp/test/mod-operations
  (:use :cl :fiveam :cp/mod-operations)
  (:import-from :cp/test/base #:base-suite))
(in-package :cp/test/mod-operations)
(in-suite base-suite)

(define-mod-operations 7)

(test mod-operations
  (macrolet ((frob (spec)
               `(locally
                    (declare (,spec mod* mod+ mod-))
                  (is (= 1 (mod*)))
                  (is (= 3 (mod* 10)))
                  (is (= 3 (mod* 2 4 10)))
                  (is (= 6 (mod* -2 4)))
                  (is (= 0 (mod+)))
                  (is (= 3 (mod+ 10)))
                  (is (= 2 (mod+ 2 4 10)))
                  (is (= 4 (mod- 2 5)))
                  (is (= 5 (mod- 2 5 13)))
                  (is (= 5 (mod- 2)))
                  (is (= 0 (mod- 0))))))
    (frob inline)
    (frob notinline))
  (let ((x 8))
    (is (= 3 (progn (incfmod x 2) x)))
    (is (= 6 (progn (decfmod x 4) x)))
    (is (= 2 (progn (mulfmod x 5) x)))))
