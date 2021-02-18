(defpackage :cp/test/integer-root
  (:use :cl :fiveam :cp/integer-root)
  (:import-from :cp/test/base #:base-suite))
(in-package :cp/test/integer-root)
(in-suite base-suite)

(test iroot/random
  (let ((*test-dribble* nil)
        (*random-state* (sb-ext:seed-random-state 0)))
    (loop for i from 1 to 100
          do (dotimes (x 100)
               (let ((root (iroot x i)))
                 (is (<= (expt root i) x))
                 (is (< x (expt (+ root 1) i)))))
             (dotimes (_ 100)
               (let* ((x (random most-positive-fixnum))
                      (root (iroot x i)))
                 (is (<= (expt root i) x))
                 (is (< x (expt (+ root 1) i))))))
    (dotimes (x 1000)
      (is (= (if (zerop x) 0 1)
             (iroot x #.(expt 10 100)))))))
