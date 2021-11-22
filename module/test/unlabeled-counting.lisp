(defpackage :cp/test/unlabeled-counting
  (:use :cl :fiveam :cp/unlabeled-counting :cp/partition-number :cp/static-mod)
  (:import-from :cp/test/base #:base-suite))
(in-package :cp/test/unlabeled-counting)
(in-suite base-suite)

(test unlabeled-deck-to-hand/partition
  (dotimes (i 50)
    (let ((seq1 (make-partition-number-sequence i +mod+))
          (seq2 (unlabeled-deck-to-hand
                 (make-array i :element-type '(unsigned-byte 31) :initial-element 1))))
      (is (equalp seq1 seq2)))))
