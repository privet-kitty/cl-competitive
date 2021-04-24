(defpackage :cp/tree-path
  (:use :cl)
  (:export #:tree-render-path))
(in-package :cp/tree-path)

(declaim (ftype (function * (values list &optional)) tree-render-path))
(defun tree-render-path (graph start end)
  "Returns the path between given two vertices."
  (declare (optimize (speed 3))
           (vector graph)
           ((mod #.array-dimension-limit) start end))
  (labels ((dfs (v parent path)
             (declare ((integer -1 (#.array-dimension-limit)) v parent))
             (when (= v end)
               (return-from tree-render-path (nreverse path)))
             (dolist (child (aref graph v))
               (unless (eql child parent)
                 (dfs child v (cons child path))))))
    (dfs start -1 (list start))))
