(defpackage :cp/test/moebius-table
  (:use :cl :fiveam :cp/moebius-table)
  (:import-from :cp/test/base #:base-suite))
(in-package :cp/test/moebius-table)
(in-suite base-suite)

(test moebius-table
  (is (equalp #(0 1 -1 -1 0 -1 1 -1 0 0 1 -1 0 -1 1 1 0 -1 0 -1 0 1 1 -1 0 0 1 0 0 -1)
              (make-moebius-table 30)))
  (is (equalp #(0 1 -1) (make-moebius-table 3)))
  (is (equalp #(0 1 -1 -1 0 -1 1 -1 0 0 1 -1 0 -1 1 1 0 -1 0 -1 0 1 1 -1 0 0 1 0 0 -1 -1 -1
                0 1 1 1 0 -1 1 1 0 -1 -1 -1 0 0 1 -1 0 0 0 1 0 -1 0 1 0 1 1 -1 0 -1 1 0)
              (cp/moebius-table::make-moebius-table 64))))
