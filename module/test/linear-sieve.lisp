(defpackage :cp/test/linear-sieve
  (:use :cl :fiveam :cp/linear-sieve)
  (:import-from :cp/test/base #:base-suite)
  (:import-from :cp/linear-sieve #:%calc-pi-upper-bound))
(in-package :cp/test/linear-sieve)
(in-suite base-suite)

(test %calc-pi-upper-bound
  (let ((*test-dribble* nil))
    (loop for x from 1 to 10000
          for incr = (if (sb-int:positive-primep x) 1 0)
          sum incr into sum
          do (is (<= incr (%calc-pi-upper-bound x))))))

(test make-minfactor-table
  (signals type-error (make-minfactor-table 0))
  (signals type-error (make-minfactor-table 1))
  (is (equalp #(0 1) (make-minfactor-table 2)))
  (is (equalp #() (nth-value 1 (make-minfactor-table 2))))
  (is (equalp #(0 1 2) (make-minfactor-table 3)))
  (is (equalp #(0 1 2 3 2 5 2 7 2 3) (make-minfactor-table 10)))
  (is (equalp #(0 1 2 3 2 5 2 7 2 3 2 11 2 13 2 3 2 17 2 19 2 3 2 23 2 5 2 3 2 29)
              (make-minfactor-table 30)))
  (is (equalp (coerce (loop for x from 2 below 30
                            when (sb-int:positive-primep x)
                            collect x)
                      'vector)
              (nth-value 1 (make-minfactor-table 30))))
  (let ((table (make-minfactor-table 200)))
    (loop for i from 2 below 200
          when (= (aref table i) i)
          do (is (sb-int:positive-primep i))
          else
          do (zerop (mod i (aref table i))))))

(test factorize-osak
  (is (equal '((2 . 2) (3 . 2) (7 . 1))
             (factorize -252 (make-minfactor-table 253))))
  (is (null (factorize 1 (make-minfactor-table 10))))
  (is (null (factorize 0 (make-minfactor-table 10))))
  (signals simple-error (factorize 252 (make-minfactor-table 252))))
