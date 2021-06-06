(defpackage :cp/dice
  (:use :cl)
  (:export #:dice #:dice-p #:copy-dice #:make-dice #:dice-rotate! #:dice-rotate)
  (:documentation "Provides six-sided dice."))
(in-package :cp/dice)

(declaim (inline %make-dice))
(defstruct (dice (:constructor %make-dice))
  (x 0 :type (integer 1 6))
  (y 0 :type (integer 1 6))
  (z 0 :type (integer 1 6)))

(declaim (inline make-dice))
(defun make-dice (x y z)
  "Makes a dice with the right-handed system: The upper surface is Z, the east
surface is X, and the north surface is Y."
  (declare ((integer 1 6) x y z))
  (assert (not (or (= (+ x y) 7)
                   (= (+ y z) 7)
                   (= (+ z x) 7))))
  (%make-dice :x x :y y :z z))

(declaim (inline dice-rotate!))
(defun dice-rotate! (dice direction)
  "DIRECTION := :EAST | :WEST | :NORTH | :SOUTH | :LEFT | :RIGHT

EAST, WEST: rotation around the Y-axis
NORTH, SOUTH: rotation around the X-axis
LEFT, RIGHT: rotation around the Z-axis"
  (ecase direction
    (:east (psetf (dice-x dice) (dice-z dice)
                  (dice-z dice) (- 7 (dice-x dice))))
    (:west (psetf (dice-z dice) (dice-x dice)
                  (dice-x dice) (- 7 (dice-z dice))))
    (:north (psetf (dice-y dice) (dice-z dice)
                   (dice-z dice) (- 7 (dice-y dice))))
    (:south (psetf (dice-z dice) (dice-y dice)
                   (dice-y dice) (- 7 (dice-z dice))))
    (:left (psetf (dice-y dice) (dice-x dice)
                  (dice-x dice) (- 7 (dice-y dice))))
    (:right (psetf (dice-x dice) (dice-y dice)
                   (dice-y dice) (- 7 (dice-x dice)))))
  dice)

(declaim (inline dice-rotate))
(defun dice-rotate (dice direction)
  (dice-rotate! (copy-dice dice) direction))
