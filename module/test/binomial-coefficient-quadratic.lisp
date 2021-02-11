(defpackage :cp/test/binomial-coefficient-quadratic
  (:use :cl :fiveam :cp/binomial-coefficient-quadratic)
  (:import-from :cp/test/base #:base-suite))
(in-package :cp/test/binomial-coefficient-quadratic)
(in-suite base-suite)

(test make-binom-table
  (is (equalp #2a((1 0 0 0 0)
                  (1 1 0 0 0)
                  (1 2 1 0 0)
                  (1 3 3 1 0)
                  (1 4 6 4 1))
              (make-binom-table 5 '(unsigned-byte 8) #'+)))
  (is (equalp #2a((1d0 0d0 0d0 0d0 0d0)
                  (1d0 1d0 0d0 0d0 0d0)
                  (1d0 2d0 1d0 0d0 0)
                  (1d0 3d0 3d0 1d0 0)
                  (1d0 4d0 6d0 4d0 1))
              (make-binom-table 5 'double-float #'+)))
  (is (equalp #2a((1)) (make-binom-table 1 'fixnum #'+)))
  (is (equalp #2a((1d0)) (make-binom-table 1 'double-float #'+))))

#+(or)
(progn
  (declaim ((simple-array (unsigned-byte 31) (* *)) *binom*))
  (sb-ext:define-load-time-global *binom*
    (make-binom-table 501
                      '(unsigned-byte 31)
                      (lambda (x y)
                        (min (+ x y) #.(ldb (byte 31 0) -1))))))
