(defpackage :cp/test/unlabeled-counting
  (:use :cl :fiveam :cp/unlabeled-counting :cp/partition-number :cp/static-mod)
  (:import-from :cp/test/base #:base-suite))
(in-package :cp/test/unlabeled-counting)
(in-suite base-suite)

(test unlabeled-deck-to-hand/partition
  (let ((*test-dribble* nil))
    (dotimes (i 50)
      (let* ((hand1 (make-partition-number-sequence i +mod+))
             (deck (make-array i :element-type '(unsigned-byte 31) :initial-element 1))
             (hand2 (unlabeled-deck-to-hand deck)))
        (is (equalp hand1 hand2))
        (when (> i 0)
          (let ((deck2 (unlabeled-hand-to-deck hand2)))
            (is (equalp (subseq deck 1 i) (subseq deck2 1 i)))))))))

(test unlabeled-deck-to-hand/random
  (dolist (n '(1 2 3 10 128 200))
    (let ((deck (make-array n :element-type '(unsigned-byte 31) :initial-element 0)))
      (dotimes (_ 200)
        (loop for i from 1 below n
              do (setf (aref deck i) (random +mod+)))
        (let* ((hand (unlabeled-deck-to-hand deck))
               (deck2 (unlabeled-hand-to-deck hand)))
          (is (equalp deck deck2)))))))
