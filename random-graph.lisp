(eval-when (:compile-toplevel :load-toplevel :execute)
  (assert (= sb-vm:n-word-bits 64)))

(declaim (inline map-random-graph))
(defun map-random-graph (function n &optional (sample 1000))
  "Applies function SAMPLE times to the adjacency matrices of random directed
graphs of N vertices, which don't contain any multiple edges but may contain
self-loops.

If what you need is an undirected graph, you can just use the upper right (or
lower left) triangle. CANONIZE-ADJACENCY-MATRIX! may be helpful."
  (declare ((integer 1 #.most-positive-fixnum) n sample)
           (function function))
  (let* ((num-words (ceiling (* n n) sb-vm:n-word-bits))
         (matrix (make-array (list n n) :element-type 'bit :initial-element 0))
         (storage (array-storage-vector matrix)))
    (declare (optimize (speed 3) (safety 0)))
    (check-type num-words (integer 0 #.most-positive-fixnum))
    (dotimes (_ sample)
      (dotimes (i num-words)
        (setf (sb-kernel:%vector-raw-bits storage i) (random #.(expt 2 64))))
      (funcall function matrix))))

(declaim (inline canonize-adjacency-matrix!))
(defun canonize-adjacency-matrix! (matrix)
  "Removes self-loops and copies the right upper triangle to the left lower
triangle. This function destructively modifies the array."
  (let ((n (array-dimension matrix 0)))
    (dotimes (i n) (setf (aref matrix i i) 0))
    (dotimes (i n)
      (dotimes (j i)
        (setf (aref matrix i j) (aref matrix j i))))
    matrix))

(defun make-random-graph (size rate &optional directed)
  "Returns a adjacency-list representation of an random generated graph."
  (declare ((mod #.array-total-size-limit) size)
           ((real 0 1) rate))
  (let ((rate (float rate 1d0))
        (graph (make-array size :element-type 'list :initial-element nil)))
    (dotimes (i size)
      (if directed
          (loop for j from 0 below size
                when (and (/= i j)
                          (< (random 1d0) rate))
                do (push j (aref graph i)))
          (loop for j from (+ i 1) below size
                when (< (random 1d0) rate)
                do (push j (aref graph i))
                   (push i (aref graph j)))))
    graph))
