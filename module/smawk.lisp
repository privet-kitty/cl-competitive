(defpackage :cp/smawk
  (:use :cl)
  (:export #:smawk))
(in-package :cp/smawk)


(declaim (inline smawk))
(defun smawk (function x1 y1 x2 y2 &key (order #'<))
  (declare (fixnum x1 y1 x2 y2))
  (let ((res (make-array (- x2 x1) :element-type 'fixnum)))
    (labels
        ((recur (xinit cols step)
           (declare (fixnum xinit step)
                    ((array fixnum (*)) cols))
           (when (>= xinit x2)
             (return-from recur (make-array 0 :element-type 'fixnum)))
           (let ((new-cols (make-array 0 :element-type 'fixnum :fill-pointer 0))
                 (end (ceiling (the fixnum (- x2 xinit)) step)))
             (declare ((mod #.array-total-size-limit) end))
             (loop
               with x of-type fixnum = (- xinit step)
               for c across cols
               do (loop 
                    while (and (/= 0 (length new-cols))
                               (funcall order
                                        (funcall function x c)
                                        (funcall function
                                                 x
                                                 (aref new-cols (- (length new-cols) 1)))))
                    do (vector-pop new-cols)
                       (decf x step))
                  (when (< (length new-cols) end)
                    (vector-push-extend c new-cols)
                    (incf x step)))
             (recur (+ xinit step) new-cols (* 2 step))
             (let ((pos 0))
               (declare ((mod #.array-total-size-limit) pos))
               (loop for x of-type fixnum from xinit below x2 by (* 2 step)
                     for end = (if (< (+ x step) x2)
                                   (aref res (- (the fixnum (+ x step)) x1))
                                   (aref new-cols (- (length new-cols) 1)))
                     for col = (aref new-cols pos)
                     do (loop while (< (aref new-cols pos) end)
                              do (incf pos)
                              when (funcall order
                                            (funcall function x (aref new-cols pos))
                                            (funcall function x col))
                              do (setq col (aref new-cols pos)))
                        (setf (aref res (- x x1)) col))
               res))))
      (let ((cols (make-array (- y2 y1) :element-type 'fixnum)))
        (dotimes (i (length cols))
          (setf (aref cols i) (+ y1 i)))
        (recur x1 cols 1)
        res))))
