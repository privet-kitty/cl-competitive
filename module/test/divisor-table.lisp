(defpackage :cp/test/divisor-table
  (:use :cl :fiveam :cp/divisor-table)
  (:import-from :cp/test/base #:base-suite))
(in-package :cp/test/divisor-table)
(in-suite base-suite)

(test make-divisors-table
  (let ((res #(() (1) (1 2) (1 3) (1 2 4) (1 5) (1 2 3 6) (1 7) (1 2 4 8) (1 3 9)
               (1 2 5 10) (1 11) (1 2 3 4 6 12))))
    (let ((lists (make-divisors-table 12 'list)))
      (is (equalp res lists))
      (is (every #'listp lists))
      (is (typep lists '(simple-array t (*)))))
    (let ((vectors (make-divisors-table 12 'vector)))
      (is (equalp (map 'simple-vector
                       (lambda (x)
                         (coerce x '(simple-array (integer 0 #.most-positive-fixnum) (*))))
                       res)
                  vectors))
      (is (every (lambda (x)
                   (typep x '(simple-array (integer 0 #.most-positive-fixnum) (*))))
                 vectors))
      (is (typep vectors '(simple-array t (*))))))
  (is (equalp #(#()) (make-divisors-table 0 'vector))))
