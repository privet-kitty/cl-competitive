(defpackage :cp/test/bipartite-matching
  (:use :cl :fiveam :cp/bipartite-matching)
  (:import-from :cp/test/base #:base-suite))
(in-package :cp/test/bipartite-matching)
(in-suite base-suite)

(test find-matcning
  (let* ((graph (make-array 9
                            :element-type 'list
                            :initial-contents '((6) (5 6 7 8) (6) (6) (5) (1 4) (0 1 2 3) (1) (1))))
         (matching (find-matching graph)))
    (loop for i below 9
          do (is-true (or (= (aref matching i) -1)
                          (= i (aref matching (aref matching i))))))
    (is (= 6 (count -1 matching :test-not #'=)))))
