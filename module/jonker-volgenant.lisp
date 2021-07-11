(defpackage :cp/jonker-volgenant
  (:use :cl)
  (:export #:solve-lap-jp #:+lap-null-vertex+ #:+lap-null-weight+
           #:lap #:make-lap #:%lap-size1 #:%lap-size2 #:%lap-matching1 #:%lap-matching2
           #:lap-p #:lap-add-edge #:lap-build #:lap-score)
  (:documentation "Provides Jonker-Volgenant algorithm for a dense instance of
assignment problem.

Reference:
Jonker, Volgenant: A shortest augmenting path algorithm for dense and sparse linear assignment problems
Jonker's C++ implementaion: https://web.archive.org/web/20070613181051/http://www.magiclogic.com/assignment/lap_cpp.zip"))
(in-package :cp/jonker-volgenant)

(defconstant +lap-null-vertex+ -1)
(defconstant +lap-null-weight+ most-positive-fixnum)

(declaim (ftype (function * (values (simple-array fixnum (*))
                                    (simple-array fixnum (*))
                                    &optional))
                solve-lap-jv))
(defun solve-lap-jv (cost-matrix)
  "Solves linear assignment problem. Returns two vectors: columns assigned to
rows and rows assigned to columns."
  (declare (optimize (speed 3))
           ((simple-array fixnum (* *)) cost-matrix))
  (let* ((n (array-dimension cost-matrix 0))
         ;; rowsol[i] = column assigned to row i
         (rowsol (make-array n :element-type 'fixnum))
         ;; colsol[j] = row assigned to column j
         (colsol (make-array n :element-type 'fixnum))
         ;; dual variables (reduced values w.r.t. column)
         (duals (make-array n :element-type 'fixnum))
         ;; stores unassigned rows
         (frees (make-array n :element-type 'fixnum))
         (end-free 0)
         ;; stores columns to be scanned
         (col-vec (make-array n :element-type 'fixnum))
         ;; counts how many times a row could be assigned
         (matches (make-array n :element-type 'fixnum :initial-element 0))
         ;; shortest distance in the (residual) network
         (dists (make-array n :element-type '(integer 0 #.most-positive-fixnum)))
         ;; row-predecessor of column in an augmenting path
         (pred (make-array n :element-type 'fixnum)))
    (declare ((integer 0 #.most-positive-fixnum) n end-free))
    (assert (= n (array-dimension cost-matrix 1)))
    (when (<= n 1)
      (return-from solve-lap-jv
        (values
         (make-array 1 :element-type 'fixnum :initial-element 0)
         (make-array 1 :element-type 'fixnum :initial-element 0))))
    ;; COLUMN REDUCTION
    (loop for j from (- n 1) downto 0
          ;; find minimum cost over rows
          for min of-type fixnum = (aref cost-matrix 0 j)
          for imin = 0
          do (loop for i from 1 below n
                   when (< (aref cost-matrix i j) min)
                   do (setq min (aref cost-matrix i j)
                            imin i))
             (setf (aref duals j) min)
             (incf (aref matches imin))
          when (= (aref matches imin) 1)
          do (setf (aref rowsol imin) j
                   (aref colsol j) imin)
          else
          do (setf (aref colsol j) +lap-null-vertex+))
    ;; REDUCTION TRANSFER
    (dotimes (i n)
      (case (aref matches i)
        (0 ;; fill FREES with unassigned rows
         (setf (aref frees end-free) i)
         (incf end-free))
        (1
         (let ((j1 (aref rowsol i)))
           (decf (aref duals j1)
                 (loop for j below n
                       when (/= j j1)
                       minimize (- (aref cost-matrix i j) (aref duals j))
                       of-type (integer 0 #.most-positive-fixnum)))))))
    ;; AUGMENTING ROW REDUCTION
    (dotimes (_ 2)
      ;; scan all free rows
      (let ((k 0)
            (prev-end-free end-free))
        (declare ((integer 0 #.most-positive-fixnum) k prev-end-free end-free))
        (setq end-free 0)
        (loop
          (when (>= k prev-end-free) (return))
          ;; find minimum and second minimum reduced cost over columns
          (let* ((i (aref frees k))
                 (umin (- (aref cost-matrix i 0) (aref duals 0)))
                 (j1 0)
                 (usubmin most-positive-fixnum)
                 (j2 0))
            (declare ((integer 0 #.most-positive-fixnum) umin usubmin i j1 j2))
            (incf k)
            (loop for j from 1 below n
                  for h of-type fixnum = (- (aref cost-matrix i j) (aref duals j))
                  do (when (< h usubmin)
                       (if (>= h umin)
                           (setq usubmin h
                                 j2 j)
                           (setq usubmin umin
                                 umin h
                                 j2 j1
                                 j1 j))))
            (let ((i0 (aref colsol j1)))
              (cond ((< umin usubmin)
                     (decf (aref duals j1) (- usubmin umin)))
                    ((/= i0 +lap-null-vertex+)
                     ;; swap columns j1 and j2
                     (setq j1 j2
                           i0 (aref colsol j2))))
              (setf (aref rowsol i) j1
                    (aref colsol j1) i)
              (unless (= i0 +lap-null-vertex+)
                (if (< umin usubmin)
                    (progn (decf k)
                           (setf (aref frees k) i0))
                    (progn (setf (aref frees end-free) i0)
                           (incf end-free)))))))))
    ;; AUGMENT SOLUTION for each free row
    (let ((min most-positive-fixnum))
      (declare ((integer 0 #.most-positive-fixnum) min))
      (dotimes (f end-free)
        (let ((free-row (aref frees f))
              (unassigned-found nil)
              (low 0)
              (up 0)
              (end 0)
              (end-of-path +lap-null-vertex+))
          (declare ((integer 0 #.most-positive-fixnum) low up end)
                   (fixnum end-of-path))
          ;; O(V^2) Dijkstra
          (dotimes (j n)
            (setf (aref dists j) (- (aref cost-matrix free-row j) (aref duals j))
                  (aref pred j) free-row
                  (aref col-vec j) j))
          (loop
            (when (= up low)
              (setq end low
                    min (aref dists (aref col-vec up)))
              (incf up)
              (loop for k from up below n
                    for j = (aref col-vec k)
                    for h = (aref dists j)
                    do (when (<= h min)
                         (when (< h min)
                           (setq up low
                                 min h))
                         (setf (aref col-vec k) (aref col-vec up)
                               (aref col-vec up) j)
                         (incf up)))
              (loop for k from low below up
                    when (= (aref colsol (aref col-vec k)) +lap-null-vertex+)
                    do (setq end-of-path (aref col-vec k)
                             unassigned-found t)
                       (return)))
            (unless unassigned-found
              (let* ((j1 (aref col-vec low))
                     (i (aref colsol j1))
                     (h (- (aref cost-matrix i j1)
                           (aref duals j1)
                           min)))
                (declare (fixnum h))
                (incf low)
                (loop for k from up below n
                      for j = (aref col-vec k)
                      for dist of-type fixnum = (- (aref cost-matrix i j)
                                                   (aref duals j)
                                                   h)
                      do (when (< dist (aref dists j))
                           (setf (aref pred j) i)
                           (when (= dist min)
                             (if (= (aref colsol j) +lap-null-vertex+)
                                 (progn
                                   (setq end-of-path j
                                         unassigned-found t)
                                   (return))
                                 (setf (aref col-vec k) (aref col-vec up)
                                       (aref col-vec up) j
                                       up (+ up 1))))
                           (setf (aref dists j) dist)))))
            (when unassigned-found (return)))
          ;; update column prices
          (dotimes (k end)
            (let ((j (aref col-vec k)))
              (incf (aref duals j)
                    (- (aref dists j) min))))
          (loop
            (let ((i (aref pred end-of-path))
                  (j end-of-path))
              (setf (aref colsol end-of-path) i
                    end-of-path (aref rowsol i)
                    (aref rowsol i) j)
              (when (= i free-row)
                (return))))))
      (values rowsol colsol))))

(defstruct (lap (:constructor make-lap
                    (size1 size2
                     &optional maximize
                     &aux (matrix (make-array (list size1 size2)
                                              :element-type 'fixnum
                                              :initial-element +lap-null-weight+))))
                (:conc-name %lap-)
                (:copier nil))
  "MAKE-LAP initializes a weighted bipartite matching problem.
SIZE1 := the number of `left' vertices
SIZE2 := the number of `right' vertices
MAZIMIZE := true [false] if maximization [minimization] problem"
  (matrix nil :type (simple-array fixnum (* *)))
  (maximize nil :type boolean)
  (size1 0 :type (mod #.array-dimension-limit))
  (size2 0 :type (mod #.array-dimension-limit))
  (matching1 nil :type (or null (simple-array fixnum (*))))
  (matching2 nil :type (or null (simple-array fixnum (*)))))

(declaim (inline lap-add-edge))
(defun lap-add-edge (lap v1 v2 weight)
  (declare (fixnum weight))
  (let ((weight (if (%lap-maximize lap) (- weight) weight)))
    (assert (< (abs weight) +lap-null-weight+))
    (setf (aref (%lap-matrix lap) v1 v2) weight)))

(declaim (ftype (function * (values (simple-array fixnum (*))
                                    (simple-array fixnum (*))
                                    &optional))
                lap-build))
(defun lap-build (lap size)
  "Computes a minimum (or maximum) weight matching of the specified
size. Returns two vectors that expresses an optimal matching: group 1 -> group
2, group 2 -> group 1"
  (declare ((mod #.array-dimension-limit) size))
  (assert (<= size (min (%lap-size1 lap) (%lap-size2 lap))))
  (let* ((size1 (%lap-size1 lap))
         (size2 (%lap-size2 lap))
         (delta (- (min size1 size2) size))
         (extended-size (+ (max size1 size2) delta))
         (matrix (%lap-matrix lap))
         (new-matrix (make-array (list extended-size extended-size)
                                 :element-type 'fixnum
                                 :initial-element 0))
         (max-weight most-negative-fixnum)
         (min-weight most-positive-fixnum))
    (declare (fixnum max-weight min-weight))
    ;; detect min. anx max. weight in MATRIX
    (dotimes (i size1)
      (dotimes (j size2)
        (unless (= (aref matrix i j) +lap-null-weight+)
          (setq max-weight (max max-weight (aref matrix i j))
                min-weight (min min-weight (aref matrix i j))))))
    (when (= max-weight most-negative-fixnum)
      (let ((matching1 (make-array size1 :element-type 'fixnum :initial-element +lap-null-vertex+))
            (matching2 (make-array size2 :element-type 'fixnum :initial-element +lap-null-vertex+)))
        (setf (%lap-matching1 lap) matching1
              (%lap-matching2 lap) matching2)
        (return-from lap-build (values matching1 matching2))))
    ;; FILL TOP LEFT
    ;; 1. add a constant to adjust all the weights to non-negative value
    ;; 2. fill null edges with infinite weight
    (let* ((offset (max 0 (- min-weight)))
           (inf-weight (+ 1 (* (+ max-weight offset) (min size1 size2)))))
      (declare ((integer 0 #.most-positive-fixnum) offset inf-weight))
      (dotimes (i size1)
        (dotimes (j size2)
          (setf (aref new-matrix i j)
                (if (= (aref matrix i j) +lap-null-weight+)
                    inf-weight
                    (+ (aref matrix i j) offset)))))
      ;; FILL BOTTOM RIGHT with infinite weight
      (loop for i from size1 below extended-size
            do (loop for j from size2 below extended-size
                     do (setf (aref new-matrix i j) inf-weight)))
      (multiple-value-bind (tmp1 tmp2) (solve-lap-jv new-matrix)
        (let ((matching1 (make-array size1
                                     :element-type 'fixnum
                                     :initial-element +lap-null-vertex+))
              (matching2 (make-array size2
                                     :element-type 'fixnum
                                     :initial-element +lap-null-vertex+)))
          (dotimes (row size1)
            (let ((col (aref tmp1 row)))
              (when (and (< col size2)
                         (/= (aref matrix row col) +lap-null-weight+))
                (setf (aref matching1 row) col))))
          (dotimes (col size2)
            (let ((row (aref tmp2 col)))
              (when (and (< row size1)
                         (/= (aref matrix row col) +lap-null-weight+))
                (setf (aref matching2 col) row))))
          (setf (%lap-matching1 lap) matching1
                (%lap-matching2 lap) matching2)
          (values matching1 matching2))))))

(declaim (ftype (function * (values integer &optional)) lap-score))
(defun lap-score (lap)
  "Computes the score of the built matching."
  (let ((matching1 (%lap-matching1 lap))
        (matrix (%lap-matrix lap))
        (result 0))
    (declare (integer result))
    (dotimes (i (%lap-size1 lap))
      (when (/= (aref matching1 i) +lap-null-vertex+)
        (incf result (aref matrix i (aref matching1 i)))))
    (if (%lap-maximize lap) (- result) result)))
