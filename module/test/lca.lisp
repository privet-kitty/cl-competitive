(defpackage :cp/test/lca
  (:use :cl :fiveam :cp/lca :cp/random-tree)
  (:import-from :cp/test/base #:base-suite))
(in-package :cp/test/lca)
(in-suite base-suite)

;; (defun make-tree (n &optional state)
;;   (let ((state (or state *random-state*)))
;;     (make-random-tree n :state state)))

(defun %tree-jump (graph start end delta)
  (labels ((dfs (v parent path)
             (if (eql v end)
                 (return-from %tree-jump
                   (nth delta (nreverse path)))
                 (dolist (child (aref graph v))
                   (unless (eql child parent)
                     (dfs child v (cons child path)))))))
    (dfs start -1 (list start))))

(test lca/hand
  (let ((*test-dribble* nil))
    (let* ((graph (make-array 8
                              :element-type 'list
                              :initial-contents '((1 2) (0 3 4) (0 5) (1) (1 6 7) (2) (4) (4))))
           (graph2 (make-array 9
                               :element-type 'list
                               :initial-contents '((1) (0 2) (1 3) (2 4) (3 5) (4 6) (5 7) (6 8) (7))))
           (table (make-lca-table graph :root 0))
           (table2 (make-lca-table graph2)))
      (is (= 4 (lca-get-lca table 6 7)))
      (is (= 1 (lca-get-lca table 3 7)))
      (is (= 0 (lca-get-lca table 3 5)))
      (is (= 0 (lca-get-lca table 5 3)))
      (is (= 4 (lca-get-lca table 4 4)))
      (is (= 5 (lca-distance table 7 5)))
      (is (= 0 (lca-distance table 4 4)))
      (is (= 1 (lca-distance table 3 1)))
      (dotimes (u 9)
        (dotimes (v 9)
          (is (= (min u v) (lca-get-lca table2 u v)))))
      (labels ((frob (graph table)
                 (dotimes (i (length graph))
                   (dotimes (j (length graph))
                     (let ((dist (lca-distance table i j)))
                       (dotimes (delta (+ dist 1))
                         (is (= (lca-jump table i j delta)
                                (%tree-jump graph i j delta)))))))))
        (frob graph table)
        (frob graph2 table2))
      (signals type-error (lca-get-lca table -1 0))))
  ;; forest
  (let ((table (make-lca-table #((3) () (6) (7 0 9) (7) () (2) (4 3) () (3)))))
    (is (equalp #(0 0 0 1 3 0 1 2 0 2) (lca-depths table)))
    (signals two-vertices-disconnected-error (lca-get-lca table 0 1))
    (signals two-vertices-disconnected-error (lca-distance table 1 2))))
