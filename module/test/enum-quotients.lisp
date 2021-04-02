(defpackage :cp/test/enum-quotients
  (:use :cl :fiveam :cp/enum-quotients)
  (:import-from :cp/test/base #:base-suite))
(in-package :cp/test/enum-quotients)
(in-suite base-suite)

(defun %identity (vec n)
  (nreverse (map 'vector (lambda (x) (+ 1 (floor n x))) vec)))

(test enum-quotients
  (let ((*test-dribble* nil))
    (is (equalp #(1 2) (enum-quotients 1)))
    (is (equalp #(1 2 3) (enum-quotients 2)))
    (is (equalp #(1 2 3 4 6 11) (enum-quotients 10)))
    (loop for x from 1 to 1000
          for vec = (enum-quotients x)
          do (is (equalp (%identity vec x) vec)))))
