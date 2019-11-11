(eval-when (:compile-toplevel :load-toplevel :execute)
  (load "test-util")
  (load "../tree-centroid.lisp"))

(use-package :test-util)

(with-test (:name tree-centroid)
  (let ((tc (make-tree-centroid #a((4) list (1) (0 2) (1 3) (2)))))
    (dotimes (root 4)
      (multiple-value-bind (c1 childs1 c2 childs2) (tc-find-centroid tc root)
        (assert (or (and (= c1 1) (= c2 2))
                    (and (= c1 2) (= c2 1))))))))
