(defpackage :cp/test/wavelet-matrix
  (:use :cl :fiveam :cp/wavelet-matrix :cp/displace :cp/compact-bit-vector)
  (:import-from :cp/test/base #:base-suite))
(in-package :cp/test/wavelet-matrix)
(in-suite base-suite)

(defun extend-to-words (bit-vector)
  (let ((n (length bit-vector)))
    (adjust-array (copy-seq bit-vector)
                  (* sb-vm:n-word-bits (ceiling n sb-vm:n-word-bits)))))

(test wavelet-empty
  (let ((w (make-wavelet-matrix 10 #())))
    (signals invalid-wavelet-index-error (wavelet-ref w 0))
    (finishes (wavelet-map-frequency (lambda (x y)
                                       (declare (ignore x y))
                                       (error "Huh?"))
                                     w 0 10))
    (finishes (wavelet-map-frequency (lambda (x y)
                                       (declare (ignore x y))
                                       (error "Huh?"))
                                     w 0 10 0 0))
    (signals invalid-wavelet-index-error
      (wavelet-map-frequency (lambda (x y)
                               (declare (ignore x y))
                               (error "Huh?"))
                             w 0 10 0 1))))

(test wavelet-matrix/manual
  ;; Miti_7's example (http://miti-7.hatenablog.com/entry/2018/04/28/152259)
  (let* ((miti7 (vector 5 4 5 5 2 1 5 6 1 3 5 0))
         (w (make-wavelet-matrix 3 miti7))
         (data (wavelet-data w)))
    (loop for sbv across data
          for ref-bits in '(#*110101111010 #*100100000010 #*111100110010)
          do (is (equal (extend-to-words ref-bits) (cbv-storage sbv))))
    (is (equalp #(4 9 5) (wavelet-zeros w)))
    
    ;; wavelet-ref
    (loop for x across miti7
          for i from 0
          do (is (= x (wavelet-ref w i))))
    (signals invalid-wavelet-index-error (wavelet-ref w 12))
    (signals type-error (wavelet-ref w -1))
    ;; wavelet-count
    (finishes
      (dotimes (x 8)
        (dotimes (l 12)
          (loop for r from l to 12
                do (assert (= (wavelet-count w x l r)
                              (count x #(5 4 5 5 2 1 5 6 1 3 5 0) :start l :end r)))))))
    ;; wavelet-kth-smallest
    (finishes
      (dotimes (l 12)
        (loop for r from (+ l 1) to 12
              do (dotimes (k (- r l))
                   (let* ((seq (vector 5 4 5 5 2 1 5 6 1 3 5 0))
                          (subseq (displace seq l r)))
                     (sort subseq #'<)
                     (assert (= (wavelet-kth-smallest w k l r)
                                (aref subseq k))))))))
    (signals error (wavelet-kth-smallest w 13))
    (signals error (wavelet-kth-smallest w 1 0 0))
    ;; Returns infinity if K is equal to the size.
    (is (= (- (expt 2 (wavelet-depth w)) 1)
           (wavelet-kth-smallest w 0 0 0)
           (wavelet-kth-smallest w 0 1 1)
           (wavelet-kth-smallest w 0 12 12)))
    (is (= (- (expt 2 7) 1)
           (wavelet-kth-smallest (make-wavelet-matrix 7 miti7) 0 0 0)
           (wavelet-kth-smallest (make-wavelet-matrix 7 miti7) 0 1 1)
           (wavelet-kth-smallest (make-wavelet-matrix 7 miti7) 0 12 12)))
    ;; wavelet-kth-largest
    (finishes
      (dotimes (l 12)
        (loop for r from (+ l 1) to 12
              do (dotimes (k (- r l))
                   (let* ((seq (vector 5 4 5 5 2 1 5 6 1 3 5 0))
                          (subseq (displace seq l r)))
                     (sort subseq #'>)
                     (assert (= (wavelet-kth-largest w k l r)
                                (aref subseq k))))))))
    (signals error (wavelet-kth-largest w 13))
    (signals error (wavelet-kth-largest w 1 0 0))
    ;; Returns zero if K is equal to the size.
    (is (= 0
           (wavelet-kth-largest w 0 0 0)
           (wavelet-kth-largest w 0 1 1)
           (wavelet-kth-largest w 0 12 12)))
    (is (= 0
           (wavelet-kth-largest (make-wavelet-matrix 7 miti7) 0 0 0)
           (wavelet-kth-largest (make-wavelet-matrix 7 miti7) 0 1 1)
           (wavelet-kth-largest (make-wavelet-matrix 7 miti7) 0 12 12)))
    ;; wavelet-range-count
    (finishes
      (dotimes (l 12)
        (loop
          for r from (+ l 1) to 12
          do (loop
               for lo from 0 to 7
               do (loop
                    for hi from lo to 7
                    do (assert (= (wavelet-range-count w lo hi l r)
                                  (count-if (lambda (x) (<= lo x (- hi 1)))
                                            miti7
                                            :start l
                                            :end r))))))))
    (signals invalid-wavelet-index-error (wavelet-range-count w 3 5 0 13))
    (signals invalid-wavelet-index-error (wavelet-range-count w 3 5 12 11))
    (signals error (wavelet-range-count w 3 2))
    ;; wavelet-map-frequency
    (finishes
      (dotimes (l 12)
        (loop
          for r from (+ l 1) to 12
          do (loop
               for lo from 0 to 7
               do (loop
                    for hi from lo to 7
                    do (let* ((seq (vector 5 4 5 5 2 1 5 6 1 3 5 0))
                              (res1 (make-array 10 :initial-element 0))
                              (res2 (make-array 10 :initial-element 0)))
                         (loop for i from l below r
                               when (<= lo (aref seq i) (- hi 1))
                               do (incf (aref res1 (aref seq i))))
                         (wavelet-map-frequency
                          (lambda (value freq) (incf (aref res2 value) freq))
                          w lo hi l r)
                         (assert (equalp res1 res2))))))))))

(test wavelet-matrix/random
  (let ((state (sb-ext:seed-random-state 0)))
    (dolist (len '(10007 1024))
      (let* ((vec (coerce (loop repeat len collect (random 500 state))
                          '(simple-array (unsigned-byte 32) (*))))
             (w (make-wavelet-matrix 9 vec)))
        (finishes
          (loop for x across vec
                for i from 0
                do (assert (= x (wavelet-ref w i)))))
        (signals invalid-wavelet-index-error (wavelet-ref w len))))))
