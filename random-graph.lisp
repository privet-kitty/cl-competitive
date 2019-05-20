(declaim (inline map-random-graph))
(defun map-random-graph (function n &optional (sample 1000))
  "Applies function SAMPLE times to the adjacency matrices of random directed
graphs of N vertices, which don't contain any multiple edges but may contain
self-loops.

If what you need is an undirected graph, you can just use the upper right (or
lower left) triangle. NORMALIZE-ADJACENCY-MATRIX! may be helpful."
  (declare ((integer 1 #.most-positive-fixnum) n sample))
  (assert (= sb-vm:n-word-bits 64))
  (let* ((num-words (floor (expt n 2) sb-vm:n-word-bits))
         (end-index (* num-words sb-vm:n-word-bits))
         (total-size (* n n))
         (mask-msb (dpb 0 (byte (- total-size end-index) 0) #.(- (expt 2 64) 1)))
         (mask-lsb (- (expt 2 (- total-size end-index)) 1))
         (matrix (make-array (list n n) :element-type 'bit :initial-element 0))
         (storage (array-storage-vector matrix)))
    (declare ((integer 0 #.most-positive-fixnum) num-words end-index total-size)
             ((unsigned-byte 64) mask-msb mask-lsb))
    (dotimes (_ sample)
      (dotimes (i num-words)
        (setf (sb-kernel:%vector-raw-bits storage i)
              (random #.(expt 2 64))))
      ;; Preserves the rest bits of the last word though it may be unnecessary.
      (unless (= end-index total-size)
        (setf (sb-kernel:%vector-raw-bits storage num-words)
              (logxor (logand mask-msb (sb-kernel:%vector-raw-bits storage num-words))
                      (logand mask-lsb (random #.(expt 2 64))))))
      (funcall function matrix))))

(declaim (inline normalize-adjacency-matrix!))
(defun normalize-adjacency-matrix! (matrix)
  "Removes self-loops and copies the right upper triangle to the left lower
triangle. This function destructively modifies the array."
  (let ((n (array-dimension matrix 0)))
    (dotimes (i n) (setf (aref matrix i i) 0))
    (dotimes (i n)
      (loop for j from 0 below i
            do (setf (aref matrix i j) (aref matrix j i))))))
