(defpackage :cp/bipartite-p
  (:use :cl)
  (:export #:bipartite-p))
(in-package :cp/bipartite-p)

;; PAY ATTENTION TO THE STACK SIZE!
(declaim (inline bipartite-p)
         (ftype (function * (values (or null simple-bit-vector) &optional)) bipartite-p))
(defun bipartite-p (graph)
  "Checks if GRAPH is bipartite and returns the vector of colorings if so,
otherwise returns NIL.

GRAPH := vector of adjacency lists"
  (declare (vector graph))
  (let* ((n (length graph))
         (visited (make-array n :element-type 'bit :initial-element 0))
         (colors (make-array n :element-type 'bit :initial-element 0)))
    (labels ((dfs (vertex color)
               (cond ((zerop (aref visited vertex))
                      (setf (aref visited vertex) 1
                            (aref colors vertex) color)
                      (if (= color 1)
                          (dolist (neighbor (aref graph vertex))
                            (dfs neighbor 0))
                          (dolist (neighbor (aref graph vertex))
                            (dfs neighbor 1))))
                     ((/= color (aref colors vertex))
                      (return-from bipartite-p nil)))))
      (dotimes (i n colors)
        (when (zerop (aref visited i))
          (dfs i 1))))))
