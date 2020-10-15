(defpackage :cp/test/sort-by-index
  (:use :cl :fiveam :cp/sort-by-index)
  (:import-from :cp/test/base #:base-suite))
(in-package :cp/test/sort-by-index)
(in-suite base-suite)

(test sort-by-index!
  (let* ((a (vector 47 8 9 84 1 0))
         (b (vector 2 0 8 9 3 4))
         (c (vector 1 8 94 7 8 9))
         (indices (sort-by-index! (lambda (i j)
                                    (<= (+ (aref a i) (aref b i) (aref c i))
                                        (+ (aref a j) (aref b j) (aref c j))))
                                  a b c)))
    (is (equalp #(1 0 8 47 84 9) a))
    (is (equalp #(3 4 0 2 9 8) b))
    (is (equalp #(8 9 8 1 7 94) c))
    (is (equalp #(4 5 1 0 3 2) indices))))
