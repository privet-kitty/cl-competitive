(defpackage :cp/test/tree-binary-lifting-edge
  (:use :cl :fiveam :cp/tree-binary-lifting-edge :cp/random-tree)
  (:import-from :cp/test/base #:base-suite))
(in-package :cp/test/tree-binary-lifting-edge)
(in-suite base-suite)

(define-tree-binary-lifting-edge tbl
  :op (lambda (s1 s2) (concatenate 'simple-base-string s1 s2))
  :identity #.(coerce "" 'simple-base-string)
  :element-type string)

(defun make-tree (n &optional state)
  (let ((state (or state *random-state*)))
    (make-random-tree
     n
     :weight-key (lambda (_ __)
                   (declare (ignore _ __))
                   (let* ((len (random 5 state))
                          (res (make-string len :element-type 'base-char)))
                     (dotimes (i len res)
                       (setf (aref res i) (code-char (+ 97 (random 26 state)))))))
     :state state)))

(defun naive-fold (graph start end)
  (labels ((dfs (v parent path)
             (if (= v end)
                 (return-from naive-fold
                   (apply #'concatenate 'simple-base-string (nreverse path)))
                 (loop for (child . s) in (aref graph v)
                       unless (eql child parent)
                       do (dfs child v (cons s path))))))
    (dfs start -1 nil)))

(test tree-binary-lifting-edge/hand
  (let* ((graph (make-array 8 :element-type 'list :initial-contents '((1 2) (0 3 4) (0 5) (1) (1 6 7) (2) (4) (4))))
         (graph2 (make-array 9 :element-type 'list :initial-contents '((1) (0 2) (1 3) (2 4) (3 5) (4 6) (5 7) (6 8) (7))))
         (table (make-tbl graph :root 0 :weight-key (constantly "")))
         (table2 (make-tbl graph2 :weight-key (constantly ""))))
    (is (= 4 (tbl-get-lca table 6 7)))
    (is (= 1 (tbl-get-lca table 3 7)))
    (is (= 0 (tbl-get-lca table 3 5)))
    (is (= 0 (tbl-get-lca table 5 3)))
    (is (= 4 (tbl-get-lca table 4 4)))
    (is (= 5 (tbl-distance table 7 5)))
    (is (= 0 (tbl-distance table 4 4)))
    (is (= 1 (tbl-distance table 3 1)))
    (dotimes (u 9)
      (dotimes (v 9)
        (is (= (min u v) (tbl-get-lca table2 u v)))))
    (signals type-error (tbl-get-lca table -1 0)))
  ;; forest
  (let ((table (make-tbl #((3) () (6) (7 0 9) (7) () (2) (4 3) () (3)) :weight-key (constantly ""))))
    (is (equalp #(0 0 0 1 3 0 1 2 0 2) (%tbl-depths table)))
    (signals two-vertices-disconnected-error (tbl-get-lca table 0 1))
    (signals two-vertices-disconnected-error (tbl-distance table 1 2))))

(test tree-binary-lifting-edge/random
  (let ((state (sb-ext:seed-random-state 0)))
    (dotimes (_ 100)
      (let* ((size (+ 1 (random 30 state)))
             (graph (make-tree size state))
             (tbl (make-tbl graph
                            :root (random size state)
                            :vertex-key #'car
                            :weight-key #'cdr)))
        (finishes
          (dotimes (_ 100)
            (let ((u (random size))
                  (v (random size)))
              (assert (equal (naive-fold graph u v)
                             (tbl-fold tbl u v))))))))))
