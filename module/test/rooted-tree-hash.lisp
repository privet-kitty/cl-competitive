(defpackage :cp/test/rooted-tree-hash
  (:use :cl :fiveam :cp/rooted-tree-hash)
  (:import-from :cp/test/base #:base-suite))
(in-package :cp/test/rooted-tree-hash)
(in-suite base-suite)

(test rooted-tree-hash
  (let ((graph1 #((1 2 3) (0) (0 5) (0 4) (3) (2 6 7) (5) (5)))
        (graph2 #((4) (6) (3) (2 7 4) (3 0) (6) (7 5 1) (3 6))))
    (dotimes (r1 8)
      (dotimes (r2 8)
        (if (member (cons r1 r2)
                    '((0 . 3) (1 . 2) (2 . 7) (3 . 4) (4 . 0) (5 . 6) (7 . 5) (6 . 1)
                      (7 . 1) (6 . 5))
                    :test #'equal)
            (is (= (rooted-tree-hash graph1 r1) (rooted-tree-hash graph2 r2)))
            (is (/= (rooted-tree-hash graph1 r1) (rooted-tree-hash graph2 r2))))))))
