(eval-when (:compile-toplevel :load-toplevel :execute)
  (load "test-util")
  (load "../bipartite-matching.lisp"))

(use-package :test-util)

(with-test (:name find-matcning)
  (let* ((graph (make-array 9
                            :element-type 'list
                            :initial-contents '((6) (5 6 7 8) (6) (6) (5) (1 4) (0 1 2 3) (1) (1))))
         (matching (find-matching graph)))
    (loop for i below 9
          do (assert (or (= (aref matching i) -1)
                         (= i (aref matching (aref matching i))))))
    (assert (= 6 (count -1 matching :test-not #'=)))))
