(defpackage :cp/unrooted-tree-hash
  (:use :cl :cp/tree-path :cp/tree-hash :cp/diameter)
  (:export #:unrooted-tree-hashes))
(in-package :cp/unrooted-tree-hash)

;; not tested
(defun unrooted-tree-hashes (graph)
  "Returns two hash values for a given unrooted tree."
  (declare (optimize (speed 3))
           (vector graph))
  (multiple-value-bind (len end1 end2) (find-diameter graph)
    (let ((path (tree-render-path graph end1 end2)))
      (values (tree-hash graph (nth (floor len 2) path))
              (tree-hash graph (nth (ceiling len 2) path))))))
