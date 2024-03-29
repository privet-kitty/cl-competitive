(defpackage :cp/test/compact-bit-vector
  (:use :cl :fiveam :cp/compact-bit-vector)
  (:import-from :cp/test/base #:base-suite))
(in-package :cp/test/compact-bit-vector)
(in-suite base-suite)

(test compact-bit-vector/hand
  (let ((cbv (make-compact-bit-vector! #*101101)))
    (is (= 0 (cbv-rank cbv 0)))
    (is (= 1 (cbv-rank cbv 1)))
    (is (= 1 (cbv-rank cbv 2)))
    (is (= 2 (cbv-rank cbv 3)))
    (is (= 3 (cbv-rank cbv 4)))
    (is (= 3 (cbv-rank cbv 5)))
    (is (= 4 (cbv-rank cbv 6)))
    ;; (signals error (cbv-rank cbv 7))
    (is (= 0 (cbv-select cbv 0)))
    (is (= 0 (cbv-select cbv 1)))
    (is (= 2 (cbv-select cbv 2)))
    (is (= 3 (cbv-select cbv 3)))
    (is (= 5 (cbv-select cbv 4)))
    (signals error (cbv-select cbv 5)))
  
  ;; null case
  (let ((cbv (make-compact-bit-vector! #*)))
    (is (= 0 (cbv-rank cbv 0)))
    (is (= 0 (cbv-select cbv 0)))))


(test compact-bit-vector/random
  (let ((state (sb-ext:seed-random-state 0)))
    (let* ((vec (let ((tmp (make-array 10000 :element-type 'bit)))
                  (dotimes (i (length tmp) tmp)
                    (setf (aref tmp i) (random 2 state)))))
           (cbv (make-compact-bit-vector! vec))
           (sum 0))
      (is (zerop (cbv-select cbv 0)))
      (finishes
        (dotimes (i (length vec))
          (assert (= sum (cbv-rank cbv i)))
          (when (= 1 (aref vec i))
            (incf sum)
            (assert (= i (cbv-select cbv sum)))))))
    
    ;; random case (multiple of word size)
    (let* ((vec (let ((tmp (make-array (* 10 sb-vm:n-word-bits) :element-type 'bit)))
                  (dotimes (i (length tmp) tmp)
                    (setf (aref tmp i) (random 2 state)))))
           (cbv (make-compact-bit-vector! vec))
           (sum 0))
      (is (zerop (cbv-select cbv 0)))
      (finishes
        (dotimes (i (length vec))
          (assert (= sum (cbv-rank cbv i)))
          (when (= 1 (aref vec i))
            (incf sum)
            (assert (= i (cbv-select cbv sum)))))))))
