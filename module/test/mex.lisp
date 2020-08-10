(defpackage :cp/test/mex
  (:use :cl :fiveam :cp/mex)
  (:import-from :cp/test/base #:base-suite))
(in-package :cp/test/mex)
(in-suite base-suite)

(test mex
  (is (= 0 (mex)))
  (is (= 0 (mex 1 2 4 7)))
  (is (= 3 (mex 0 1 2 4 7)))
  (is (= 5 (mex 0 1 2 3 4))))
