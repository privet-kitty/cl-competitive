(defpackage :cp/test/copy-array
  (:use :cl :fiveam :cp/copy-array)
  (:import-from :cp/test/base #:base-suite))
(in-package :cp/test/copy-array)
(in-suite base-suite)

(test copy-array/hand
  (let* ((arr #2a((1 2 3) (4 5 6.0)))
         (copied (copy-array arr)))
    (is (equalp copied #2a((1 2 3) (4 5 6.0))))
    (is (not (eq arr copied)))))
