(eval-when (:compile-toplevel :load-toplevel :execute)
  (load "test-util")
  (load "../wavelet-matrix.lisp")
  (load "../displace.lisp"))

(use-package :test-util)

(defparameter *seed* (seed-random-state 0))

(defun extend-to-chunk (bit-vector)
  (let ((n (length bit-vector)))
    (adjust-array (copy-seq bit-vector)
                  (* +chunk-width+ (ceiling n +chunk-width+)))))

(with-test (:name succinct-bit-vector)
  (let ((sucbv (make-sucbv! #*101101)))
    (assert (= 0 (sucbv-rank sucbv 0)))
    (assert (= 1 (sucbv-rank sucbv 1)))
    (assert (= 1 (sucbv-rank sucbv 2)))
    (assert (= 2 (sucbv-rank sucbv 3)))
    (assert (= 3 (sucbv-rank sucbv 4)))
    (assert (= 3 (sucbv-rank sucbv 5)))
    (assert (= 4 (sucbv-rank sucbv 6)))
    ;; (signals error (sucbv-rank sucbv 7))
    (assert (= 0 (sucbv-select sucbv 0)))
    (assert (= 0 (sucbv-select sucbv 1)))
    (assert (= 2 (sucbv-select sucbv 2)))
    (assert (= 3 (sucbv-select sucbv 3)))
    (assert (= 5 (sucbv-select sucbv 4)))
    (signals error (sucbv-select sucbv 5)))
  
  ;; null case
  (let ((sucbv (make-sucbv! #*)))
    (assert (= 0 (sucbv-rank sucbv 0)))
    (assert (= 0 (sucbv-select sucbv 0))))

  ;; random case (non-multiple of +chunk-width+)
  (let* ((vec (let ((tmp (make-array 10000 :element-type 'bit)))
                (dotimes (i (length tmp) tmp)
                  (setf (aref tmp i) (random 2 *seed*)))))
         (sucbv (make-sucbv! vec))
         (sum 0))
    (assert (zerop (sucbv-select sucbv 0)))
    (dotimes (i (length vec))
      (assert (= sum (sucbv-rank sucbv i)))
      (when (= 1 (aref vec i))
        (incf sum)
        (assert (= i (sucbv-select sucbv sum))))))
  
  ;; random case (multiple of +chunk-width+)
  (let* ((vec (let ((tmp (make-array +chunk-width+ :element-type 'bit)))
                (dotimes (i (length tmp) tmp)
                  (setf (aref tmp i) (random 2 *seed*)))))
         (sucbv (make-sucbv! vec))
         (sum 0))
    (assert (zerop (sucbv-select sucbv 0)))
    (dotimes (i (length vec))
      (assert (= sum (sucbv-rank sucbv i)))
      (when (= 1 (aref vec i))
        (incf sum)
        (assert (= i (sucbv-select sucbv sum)))))))

(with-test (:name wavelet-matrix)
  ;; Mitl_7's example
  (let* ((mitl7 (vector 5 4 5 5 2 1 5 6 1 3 5 0))
         (w (make-wavelet 3 mitl7))
         (data (wavelet-data w)))
    (loop for sbv across data
          for ref-bits in '(#*110101111010 #*100100000010 #*111100110010)
          do (assert (equal (extend-to-chunk ref-bits) (sucbv-storage sbv))))
    (assert (equalp #(4 9 5) (wavelet-zeros w)))
    
    ;; wavelet-ref
    (loop for x across mitl7
          for i from 0
          do (assert (= x (wavelet-ref w i))))
    (signals invalid-wavelet-index-error (wavelet-ref w 12))
    (signals type-error (wavelet-ref w -1))
    ;; wavelet-count
    (dotimes (x 8)
      (dotimes (l 12)
        (loop for r from l to 12
              do (assert (= (wavelet-count w x l r)
                            (count x #(5 4 5 5 2 1 5 6 1 3 5 0) :start l :end r))))))
    ;; wavelet-kth-smallest
    (dotimes (l 12)
      (loop for r from (+ l 1) to 12
            do (dotimes (k (- r l))
                 (let* ((seq (vector 5 4 5 5 2 1 5 6 1 3 5 0))
                        (subseq (displace seq l r)))
                   (sort subseq #'<)
                   (assert (= (wavelet-kth-smallest w k l r)
                              (aref subseq k)))))))
    (signals error (wavelet-kth-smallest w 13))
    (signals error (wavelet-kth-smallest w 1 0 0))
    ;; Returns infinity if K is equal to the size.
    (assert (= (- (expt 2 (wavelet-depth w)) 1)
               (wavelet-kth-smallest w 0 0 0)
               (wavelet-kth-smallest w 0 1 1)
               (wavelet-kth-smallest w 0 12 12)))
    (assert (= (- (expt 2 7) 1)
               (wavelet-kth-smallest (make-wavelet 7 mitl7) 0 0 0)
               (wavelet-kth-smallest (make-wavelet 7 mitl7) 0 1 1)
               (wavelet-kth-smallest (make-wavelet 7 mitl7) 0 12 12)))
    ;; wavelet-kth-largest
    (dotimes (l 12)
      (loop for r from (+ l 1) to 12
            do (dotimes (k (- r l))
                 (let* ((seq (vector 5 4 5 5 2 1 5 6 1 3 5 0))
                        (subseq (displace seq l r)))
                   (sort subseq #'>)
                   (assert (= (wavelet-kth-largest w k l r)
                              (aref subseq k)))))))
    (signals error (wavelet-kth-largest w 13))
    (signals error (wavelet-kth-largest w 1 0 0))
    ;; Returns zero if K is equal to the size.
    (assert (= 0
               (wavelet-kth-largest w 0 0 0)
               (wavelet-kth-largest w 0 1 1)
               (wavelet-kth-largest w 0 12 12)))
    (assert (= 0
               (wavelet-kth-largest (make-wavelet 7 mitl7) 0 0 0)
               (wavelet-kth-largest (make-wavelet 7 mitl7) 0 1 1)
               (wavelet-kth-largest (make-wavelet 7 mitl7) 0 12 12))))
  ;; random case
  (let* ((vec (coerce (loop repeat 10007 collect (random 500))
                      '(simple-array (unsigned-byte 32) (*))))
         (w (make-wavelet 9 vec)))
    (loop for x across vec
          for i from 0
          do (assert (= x (wavelet-ref w i))))
    (signals invalid-wavelet-index-error (wavelet-ref w 10007))))
