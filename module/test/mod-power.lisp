(defpackage :cp/test/mod-power
  (:use :cl :fiveam :cp/mod-power)
  (:import-from :cp/test/base #:base-suite))
(in-package :cp/test/mod-power)
(in-suite base-suite)

(defun naive-mod-power (x k m)
  (let ((res (mod 1 m)))
    (dotimes (_ k)
      (setq res (mod (* res x) m)))
    res))

(test mod-power
  (finishes
    (dotimes (i 100)
      (assert (= (mod (expt -3 i) 998244353)
                 (mod-power -3 i 998244353)))))
  (finishes
    (dotimes (x 50)
      (dotimes (k 50)
        (loop for m from 1 below 50
              do (assert (= (mod-power x k m) (naive-mod-power x k m))))))))
