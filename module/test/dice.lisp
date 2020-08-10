(defpackage :cp/test/dice
  (:use :cl :fiveam :cp/dice)
  (:import-from :cp/test/base #:base-suite))
(in-package :cp/test/dice)
(in-suite base-suite)

(test dice
  (let ((dice (make-dice 3 5 1)))
    (is (equalp (make-dice 1 5 4) (dice-rotate dice :east)))
    (is (equalp (make-dice 2 3 1) (dice-rotate dice :left)))
    (is (equalp (make-dice 3 1 2) (dice-rotate dice :north)))
    (is (equalp (make-dice 5 4 1) (dice-rotate dice :right)))
    (is (equalp (make-dice 3 6 5) (dice-rotate dice :south)))
    (is (equalp (make-dice 6 5 3) (dice-rotate dice :west)))

    (dice-rotate! dice :east)
    (is (equalp (make-dice 1 5 4) dice))))
