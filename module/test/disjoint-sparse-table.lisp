(defpackage :cp/test/disjoint-sparse-table
  (:use :cl :fiveam :cp/disjoint-sparse-table)
  (:import-from :cp/test/base #:base-suite))
(in-package :cp/test/disjoint-sparse-table)
(in-suite base-suite)

(test disjoint-sparse-table
  (declare (notinline dst-fold))
  (let ((table (make-disjoint-sparse-table #(1 2 3 4 5) #'*)))
    (is (= 1 (dst-fold table #'* 0 1)))
    (is (= 2 (dst-fold table #'* 0 2)))
    (is (= 6 (dst-fold table #'* 0 3)))
    (is (= 24 (dst-fold table #'* 0 4)))
    (is (= 120 (dst-fold table #'* 0 5)))
    (is (= 2 (dst-fold table #'* 1 2)))
    (is (= 6 (dst-fold table #'* 1 3)))
    (is (= 24 (dst-fold table #'* 1 4)))
    (is (= 120 (dst-fold table #'* 1 5)))
    (is (= 3 (dst-fold table #'* 2 3)))
    (is (= 12 (dst-fold table #'* 2 4)))
    (is (= 60 (dst-fold table #'* 2 5)))
    (is (= 4 (dst-fold table #'* 3 4)))
    (is (= 20 (dst-fold table #'* 3 5)))
    (is (= 5 (dst-fold table #'* 4 5)))
    (is (null (dst-fold table #'* 4 4)))
    (is (= 0 (dst-fold table #'* 4 4 0)))
    (signals error (dst-fold table #'* 4 3)))
  ;; no element
  (let ((table0 (make-disjoint-sparse-table #() #'gcd)))
    (is (equalp #2a(()) table0))
    (is (zerop (dst-fold table0 #'gcd 0 0 0))))
  ;; one element
  (let ((table1 (make-disjoint-sparse-table #(1) #'min)))
    (is (= 1 (dst-fold table1 #'min 0 1)))
    (is (null (dst-fold table1 #'min 0 0))))

  (let* ((state (sb-ext:seed-random-state 0))
         (n 30)
         (seq (apply #'vector (loop repeat n collect (random 1000 state))))
         (table (make-disjoint-sparse-table seq #'+)))
    (finishes
      (dotimes (l n)
        (loop for r from (+ l 1) to n
              do (assert (= (loop for i from l below r sum (aref seq i))
                            (dst-fold table #'+ l r))))))))

