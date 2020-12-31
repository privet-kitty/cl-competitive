(defpackage :cp/test/pfactors-table
  (:use :cl :fiveam :cp/pfactors-table)
  (:import-from :cp/test/base #:base-suite))
(in-package :cp/test/pfactors-table)
(in-suite base-suite)

(test make-pfactors-table
  (is (equalp #(() () ((2 . 1)) ((3 . 1)) ((2 . 2))
                ((5 . 1)) ((2 . 1) (3 . 1)) ((7 . 1)) ((2 . 3)) ((3 . 2))
                ((2 . 1) (5 . 1)) ((11 . 1)) ((2 . 2) (3 . 1)) ((13 . 1)) ((2 . 1) (7 . 1))
                ((3 . 1) (5 . 1)) ((2 . 4)) ((17 . 1)) ((2 . 1) (3 . 2)) ((19 . 1)))
              (make-pfactors-table 20)))
  (is (equalp #() (make-pfactors-table 0)))
  (is (equalp #(()) (make-pfactors-table 1))))
