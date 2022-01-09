(defpackage :cp/kd-tree
  (:use :cl :cp/quickselect)
  (:export #:kdnode #:make-kdtree #:kd-map-disk))
(in-package :cp/kd-tree)

;; NOTE: not tesded

(deftype int () '(unsigned-byte 31))

(defstruct kdnode
  (indices nil :type (simple-array (unsigned-byte 31) (*)))
  (lnode nil :type (or null kdnode))
  (rnode nil :type (or null kdnode))
  (xmin nil :type int)
  (xmax nil :type int)
  (ymin nil :type int)
  (ymax nil :type int))

(defun make-kdtree (length xkey ykey)
  (declare (optimize (speed 3))
           ((mod #.array-dimension-limit) length)
           (function xkey ykey))
  (assert (>= length 1))
  (labels
      ((recur (indices)
         (declare ((simple-array (unsigned-byte 31) (*)) indices))
         (cond ((= 0 (length indices)))
               ((= 1 (length indices))
                (let* ((i (aref indices 0))
                       (x (funcall xkey i))
                       (y (funcall ykey i)))
                  (make-kdnode :indices indices
                               :xmin x :xmax x
                               :ymin y :ymax y)))
               (t
                (loop
                  with mid = (ash (length indices) -1)
                  for i across indices
                  for x of-type int = (funcall xkey i)
                  for y of-type int = (funcall ykey i)
                  minimize x into xmin
                  maximize x into xmax
                  minimize y into ymin
                  maximize y into ymax
                  finally
                  ;; TODO: This was superior to simply choosing the two axes
                  ;; alternately. Further study is needed.
                     (if (>= (- xmax xmin) (- ymax ymin))
                         (quickselect!
                          indices
                          (lambda (i j)
                            (< (the int (funcall xkey i)) (the int (funcall xkey j))))
                          mid)
                         (quickselect!
                          indices
                          (lambda (i j)
                            (< (the int (funcall ykey i)) (the int (funcall ykey j))))
                          mid))
                     (return
                       (make-kdnode :indices indices
                                    :lnode (recur (subseq indices 0 mid))
                                    :rnode (recur (subseq indices mid))
                                    :xmin xmin :xmax xmax :ymin ymin :ymax ymax)))))))
    (let ((indices (make-array length :element-type '(unsigned-byte 31))))
      (dotimes (i length)
        (setf (aref indices i) i))
      (recur indices))))

(declaim (inline %abs2))
(defun %abs2 (x y)
  (+ (* x x) (* y y)))

(declaim (inline kd-map-disk))
(defun kd-map-disk (kdnode function x y dist)
  "Applies FUNCTION to all the points within a distance DIST from a given
point (X, Y). FUNCTION takes one argument: the index of the point."
  (declare (int x y dist))
  (let ((dist^2 (* dist dist)))
    (labels
        ((recur (kdnode)
           (let ((xmin (kdnode-xmin kdnode))
                 (xmax (kdnode-xmax kdnode))
                 (ymin (kdnode-ymin kdnode))
                 (ymax (kdnode-ymax kdnode)))
             (cond ((<= (%abs2 (max (abs (- x xmin)) (abs (- x xmax)))
                               (max (abs (- y ymin)) (abs (- y ymax))))
                        dist^2)
                    (loop for index across (kdnode-indices kdnode)
                          do (funcall function index)))
                   ((<= (%abs2 (max (- x xmax) (- xmin x) 0)
                               (max (- y ymax) (- ymin y) 0))
                        dist^2)
                    (recur (kdnode-lnode kdnode))
                    (recur (kdnode-rnode kdnode)))))))
      (recur kdnode))))
