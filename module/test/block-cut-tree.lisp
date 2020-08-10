(defpackage :cp/test/block-cut-tree
  (:use :cl :fiveam :cp/block-cut-tree)
  (:import-from :cp/test/base #:base-suite))
(in-package :cp/test/block-cut-tree)
(in-suite base-suite)

(defun set-equal (seq1 seq2)
  (let ((table1 (make-hash-table :test #'equal))
        (table2 (make-hash-table :test #'equal)))
    (loop for x being each element of seq1
          do (setf (gethash x table1) t))
    (loop for x being each element of seq2
          do (setf (gethash x table2) t))
    (and (loop for x being each element of seq1
               always (gethash x table2))
         (loop for x being each element of seq2
               always (gethash x table1)))))

(test block-cut-tree
  ;; Graph example is from http://yazaten.hatenablog.com/entry/2016/12/05/094725
  (let* ((graph #(() (2 5) (1 3) (2 4 5) (3) (1 3 7 8) (8) (5 8) (5 6 7 9) (8)))
         (bcc (make-bcc graph))
         (comps (bcc-components bcc))
         (sizes (bcc-sizes bcc))
         (bridges (bcc-bridges bcc)))
    (is (set-equal '((3 . 4) (6 . 8) (8 . 9)) bridges))
    (is (= (aref comps 1)
           (aref comps 2)
           (aref comps 3)
           (aref comps 5)
           (aref comps 7)
           (aref comps 8)))
    (is (/= (aref comps 0) (aref comps 1) (aref comps 4) (aref comps 6) (aref comps 9)))
    (is (= 6 (aref sizes (aref comps 1))))
    (is (= 1 (aref sizes (aref comps 0))))
    (is (= 1 (aref sizes (aref comps 4))))
    (is (= 1 (aref sizes (aref comps 6))))
    (is (= 1 (aref sizes (aref comps 9)))))
  ;; empty graph
  (finishes (make-bcc #())))
