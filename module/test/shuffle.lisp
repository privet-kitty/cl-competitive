(defpackage :cp/test/shuffle
  (:use :cl :fiveam :cp/shuffle :cp/set-equal)
  (:import-from :cp/test/base #:base-suite))
(in-package :cp/test/shuffle)
(in-suite base-suite)

(test shuffle
  (let ((seq #(0 1 2 3 4)))
    (dotimes (_ 100)
      (is (set-equal seq (shuffle! (copy-seq seq))))))
  (let ((seq #(0 1 2 3 4)))
    (loop for start to 5
          do (loop for end from start to 5
                   do (finishes
                        (dotimes (_ 100)
                          (let ((shuffled (shuffle! (copy-seq seq) start end)))
                            (loop for i below start
                                  do (assert (= i (aref shuffled i))))
                            (loop for i from end below (length seq)
                                  do (assert (= i (aref shuffled i))))
                            (assert (set-equal seq shuffled))))))))
  (is (set-equal (vector) (shuffle! (vector)))))
