(eval-when (:compile-toplevel :load-toplevel :execute)
  (load "test-util")
  (load "../mod-power.lisp"))

(use-package :test-util)

(defun naive-mod-power (x k m)
  (let ((res (mod 1 m)))
    (dotimes (_ k)
      (setq res (mod (* res x) m)))
    res))

(with-test (:name mod-power)
  (dotimes (i 100)
    (assert (= (mod (expt -3 i) 998244353)
               (mod-power -3 i 998244353))))
  (dotimes (x 50)
    (dotimes (k 50)
      (loop for m from 1 below 50
            do (assert (= (mod-power x k m) (naive-mod-power x k m)))))))
