(eval-when (:compile-toplevel :load-toplevel :execute)
  (load "test-util")
  (load "../wavelet-matrix.lisp")
  (load "../displace.lisp"))

(use-package :test-util)

(defparameter *state* (seed-random-state 0))

(defun extend-to-words (bit-vector)
  (let ((n (length bit-vector)))
    (adjust-array (copy-seq bit-vector)
                  (* sb-vm:n-word-bits (ceiling n sb-vm:n-word-bits)))))

(with-test (:name compact-bit-vector)
  (let ((cbv (make-compact-bit-vector! #*101101)))
    (assert (= 0 (cbv-rank cbv 0)))
    (assert (= 1 (cbv-rank cbv 1)))
    (assert (= 1 (cbv-rank cbv 2)))
    (assert (= 2 (cbv-rank cbv 3)))
    (assert (= 3 (cbv-rank cbv 4)))
    (assert (= 3 (cbv-rank cbv 5)))
    (assert (= 4 (cbv-rank cbv 6)))
    ;; (signals error (cbv-rank cbv 7))
    (assert (= 0 (cbv-select cbv 0)))
    (assert (= 0 (cbv-select cbv 1)))
    (assert (= 2 (cbv-select cbv 2)))
    (assert (= 3 (cbv-select cbv 3)))
    (assert (= 5 (cbv-select cbv 4)))
    (signals error (cbv-select cbv 5)))
  
  ;; null case
  (let ((cbv (make-compact-bit-vector! #*)))
    (assert (= 0 (cbv-rank cbv 0)))
    (assert (= 0 (cbv-select cbv 0))))

  ;; random case (non-multiple of word size)
  (let* ((vec (let ((tmp (make-array 10000 :element-type 'bit)))
                (dotimes (i (length tmp) tmp)
                  (setf (aref tmp i) (random 2 *state*)))))
         (cbv (make-compact-bit-vector! vec))
         (sum 0))
    (assert (zerop (cbv-select cbv 0)))
    (dotimes (i (length vec))
      (assert (= sum (cbv-rank cbv i)))
      (when (= 1 (aref vec i))
        (incf sum)
        (assert (= i (cbv-select cbv sum))))))
  
  ;; random case (multiple of word size)
  (let* ((vec (let ((tmp (make-array (* 10 sb-vm:n-word-bits) :element-type 'bit)))
                (dotimes (i (length tmp) tmp)
                  (setf (aref tmp i) (random 2 *state*)))))
         (cbv (make-compact-bit-vector! vec))
         (sum 0))
    (assert (zerop (cbv-select cbv 0)))
    (dotimes (i (length vec))
      (assert (= sum (cbv-rank cbv i)))
      (when (= 1 (aref vec i))
        (incf sum)
        (assert (= i (cbv-select cbv sum)))))))

(with-test (:name wavelet-empty)
  (let ((w (make-wavelet 10 #())))
    (signals invalid-wavelet-index-error (wavelet-ref w 0))
    (wavelet-map-frequency (lambda (x y) (error "Huh?")) w 0 10)
    (wavelet-map-frequency (lambda (x y) (error "Huh?")) w 0 10 0 0)
    (signals invalid-wavelet-index-error
      (wavelet-map-frequency (lambda (x y) (error "Huh?")) w 0 10 0 1))))

(with-test (:name wavelet-matrix/manual)
  ;; Miti_7's example (http://miti-7.hatenablog.com/entry/2018/04/28/152259)
  (let* ((miti7 (vector 5 4 5 5 2 1 5 6 1 3 5 0))
         (w (make-wavelet 3 miti7))
         (data (wavelet-data w)))
    (loop for sbv across data
          for ref-bits in '(#*110101111010 #*100100000010 #*111100110010)
          do (assert (equal (extend-to-words ref-bits) (cbv-storage sbv))))
    (assert (equalp #(4 9 5) (wavelet-zeros w)))
    
    ;; wavelet-ref
    (loop for x across miti7
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
               (wavelet-kth-smallest (make-wavelet 7 miti7) 0 0 0)
               (wavelet-kth-smallest (make-wavelet 7 miti7) 0 1 1)
               (wavelet-kth-smallest (make-wavelet 7 miti7) 0 12 12)))
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
               (wavelet-kth-largest (make-wavelet 7 miti7) 0 0 0)
               (wavelet-kth-largest (make-wavelet 7 miti7) 0 1 1)
               (wavelet-kth-largest (make-wavelet 7 miti7) 0 12 12)))
    ;; wavelet-range-count
    (dotimes (l 12)
      (loop
        for r from (+ l 1) to 12
        do (loop
             for lo from 0 to 7
             do (loop
                  for hi from lo to 7
                  do (let* ((seq (vector 5 4 5 5 2 1 5 6 1 3 5 0)))
                       (assert (= (wavelet-range-count w lo hi l r)
                                  (count-if (lambda (x) (<= lo x (- hi 1)))
                                            miti7
                                            :start l
                                            :end r))))))))
    (signals invalid-wavelet-index-error (wavelet-range-count w 3 5 0 13))
    (signals invalid-wavelet-index-error (wavelet-range-count w 3 5 12 11))
    (signals error (wavelet-range-count w 3 2))
    ;; wavelet-map-frequency
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
                       (assert (equalp res1 res2)))))))))

(with-test (:name wavelet-matrix/random)
  (dolist (len '(10007 1024))
    (let* ((vec (coerce (loop repeat len collect (random 500 *state*))
                        '(simple-array (unsigned-byte 32) (*))))
           (w (make-wavelet 9 vec)))
      (loop for x across vec
            for i from 0
            do (assert (= x (wavelet-ref w i))))
      (signals invalid-wavelet-index-error (wavelet-ref w len)))))
