(defpackage :cp/hackenbush
  (:use :cl)
  (:export #:calc-game-value-for-tree #:calc-game-value-for-stalk)
  (:documentation
   "Provides computation of game value of hackenbush.

Reference:
http://www.geometer.org/mathcircles/hackenbush.pdf"))
(in-package :cp/hackenbush)

(declaim (inline add-blue)
         (ftype (function * (values rational &optional)) add-blue))
(defun add-blue (x)
  (let ((stage (max 0 (floor (- 1 x)))))
    (/ (+ x stage 1) (ash 1 stage))))

(declaim (inline add-red)
         (ftype (function * (values rational &optional)) add-red))
(defun add-red (x)
  (let ((stage (max 0 (floor (+ x 1)))))
    (/ (- x stage 1) (ash 1 stage))))

(declaim (inline calc-game-value-for-tree)
         (ftype (function * (values rational &optional)) calc-game-value-for-tree))
(defun calc-game-value-for-tree (graph root &key (vertex-key #'car) (color-key #'cdr))
  "Returns the game value of a given tree. Each color must be 1 (blue) or
-1 (red)."
  (labels ((dfs (v parent)
             (let ((res 0))
               (declare (rational res))
               (dolist (edge (aref graph v))
                 (let ((child (funcall vertex-key edge))
                       (color (funcall color-key edge)))
                   (unless (eql child parent)
                     (let ((value (dfs child v)))
                       (ecase color
                         (1 (incf res (add-blue value)))
                         (-1 (incf res (add-red value))))))))
               res)))
    (dfs root -1)))

(declaim (inline calc-game-value-for-stalk)
         (ftype (function * (values rational &optional)) calc-game-value-for-stalk))
(defun calc-game-value-for-stalk (vector)
  "Returns the game value of a given stalk. Each color must be 1 (blue) or
-1 (red)."
  (let ((res 0))
    (declare (rational res))
    (loop for x from (- (length vector) 1) downto 0
          do (ecase (aref vector x)
               (1 (setq res (add-blue res)))
               (-1 (setq res (add-red res)))))
    res))
