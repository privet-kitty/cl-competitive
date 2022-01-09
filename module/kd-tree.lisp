(defpackage :cp/kd-tree
  (:use :cl :cp/quickselect)
  (:export #:kdnode #:make-kdtree #:kd-map-disk))
(in-package :cp/kd-tree)

(deftype int () '(unsigned-byte 31))

(defstruct kdnode
  (l nil :type (mod #.array-dimension-limit))
  (r nil :type (mod #.array-dimension-limit))
  (lnode nil :type (or null kdnode))
  (rnode nil :type (or null kdnode))
  (xmin nil :type int)
  (xmax nil :type int)
  (ymin nil :type int)
  (ymax nil :type int))

(defstruct (kdtree (:constructor %make-kdtree))
  (indices nil :type (simple-array (unsigned-byte 31) (*)))
  (root nil :type (or null kdnode)))

(defun make-kdtree (length xkey ykey)
  (declare (optimize (speed 3))
           ((mod #.array-dimension-limit) length)
           (function xkey ykey))
  (let ((indices (make-array length :element-type '(unsigned-byte 31))))
    (dotimes (i length)
      (setf (aref indices i) i))
    (labels
        ((recur (l r)
           (declare ((mod #.array-dimension-limit) l r))
           (cond ((= l r) nil)
                 ((= (+ l 1) r)
                  (let* ((index (aref indices l))
                         (x (funcall xkey index))
                         (y (funcall ykey index)))
                    (make-kdnode :l l :r r
                                 :xmin x :xmax x
                                 :ymin y :ymax y)))
                 (t
                  (loop
                    with mid = (ash (+ l r) -1)
                    for pos from l below r
                    for i = (aref indices pos)
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
                            (- mid l) l r)
                           (quickselect!
                            indices
                            (lambda (i j)
                              (< (the int (funcall ykey i)) (the int (funcall ykey j))))
                            (- mid l) l r))
                       (return
                         (make-kdnode :l l :r r
                                      :lnode (recur l mid)
                                      :rnode (recur mid r)
                                      :xmin xmin :xmax xmax :ymin ymin :ymax ymax)))))))
      (%make-kdtree :indices indices :root (recur 0 length)))))

(declaim (inline %abs2))
(defun %abs2 (x y)
  (+ (* x x) (* y y)))

(declaim (inline kd-map-disk))
(defun kd-map-disk (kdtree function x y dist)
  "Applies FUNCTION to all the points within a distance DIST from a given
point (X, Y). FUNCTION takes one argument: the index of the point."
  (declare (int x y dist))
  (let ((dist^2 (* dist dist))
        (indices (kdtree-indices kdtree)))
    (labels
        ((recur (kdnode)
           (let ((xmin (kdnode-xmin kdnode))
                 (xmax (kdnode-xmax kdnode))
                 (ymin (kdnode-ymin kdnode))
                 (ymax (kdnode-ymax kdnode)))
             (cond ((<= (%abs2 (max (abs (- x xmin)) (abs (- x xmax)))
                               (max (abs (- y ymin)) (abs (- y ymax))))
                        dist^2)
                    (loop for pos from (kdnode-l kdnode) below (kdnode-r kdnode)
                          for index = (aref indices pos)
                          do (funcall function index)))
                   ((<= (%abs2 (max (- x xmax) (- xmin x) 0)
                               (max (- y ymax) (- ymin y) 0))
                        dist^2)
                    (recur (kdnode-lnode kdnode))
                    (recur (kdnode-rnode kdnode)))))))
      (recur (kdtree-root kdtree)))))
