(defpackage :cp/test/lca-edge
  (:use :cl :fiveam :cp/lca-edge :cp/random-tree)
  (:import-from :cp/test/base #:base-suite))
(in-package :cp/test/lca-edge)
(in-suite base-suite)

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

(test lca-edge/random
  (let ((state (sb-ext:seed-random-state 0)))
    (dotimes (_ 100)
      (let* ((size (+ 1 (random 30 state)))
             (graph (make-tree size state))
             (lca-table (make-lca-table graph
                                        :root (random size state)
                                        :vertex-key #'car
                                        :weight-key #'cdr)))
        (finishes
          (dotimes (_ 100)
            (let ((u (random size))
                  (v (random size)))
              (assert (equal (naive-fold graph u v)
                             (lca-fold lca-table u v))))))))))
