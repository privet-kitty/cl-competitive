(eval-when (:compile-toplevel :load-toplevel :execute)
  (load "test-util")
  (load "../dice.lisp"))

(use-package :test-util)

(with-test (:name dice)
  (let ((dice (make-dice 3 5 1)))
    (assert (equalp #S(dice :x 1 :y 5 :z 4) (dice-rotate dice :east)))
    (assert (equalp #S(dice :x 2 :y 3 :z 1) (dice-rotate dice :left)))
    (assert (equalp #S(dice :x 3 :y 1 :z 2) (dice-rotate dice :north)))
    (assert (equalp #S(dice :x 5 :y 4 :z 1) (dice-rotate dice :right)))
    (assert (equalp #S(dice :x 3 :y 6 :z 5) (dice-rotate dice :south)))
    (assert (equalp #S(dice :x 6 :y 5 :z 3) (dice-rotate dice :west)))

    (dice-rotate! dice :east)
    (assert (equalp #S(dice :x 1 :y 5 :z 4) dice))))
