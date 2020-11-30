(defpackage :cp/tree-hash
  (:use :cl :cp/tree-path :cp/rooted-tree-hash :cp/diameter)
  (:export #:tree-hashes))
(in-package :cp/tree-hash)

;; not tested
(defun tree-hashes (graph)
  "Returns two hash values for a given non-rooted tree."
  (declare (optimize (speed 3))
           (vector graph))
  (multiple-value-bind (len end1 end2) (find-diameter graph)
    (let ((path (tree-render-path graph end1 end2)))
      (values (rooted-tree-hash graph (nth (floor len 2) path))
              (rooted-tree-hash graph (nth (ceiling len 2) path))))))
