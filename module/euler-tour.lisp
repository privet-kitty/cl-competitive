;; PAY ATTENTION TO THE STACK SIZE!
(declaim (inline make-euler tour))
(defun make-euler-tour (tree &key (key #'identity) (root 0))
  "Maps each vertex of (rooted) TREE to the index obtained by Euler tour. This
function returns three vectors: TOUR, PRE, POST; PRE[i] is the first index of
the vertex i on Euler tour; POST[i] is the last index.

KEY is called to each node of adjacency list and must return the index number of
the vertex.

Note that this function doesn't check if TREE is really a tree. The behaviour
for a non-tree is undefined.

TREE := vector of adjacency lists"
  (declare (vector tree)
           (function key)
           ((integer 0 #.most-positive-fixnum) root))
  (when (zerop (length tree))
    (return-from make-euler-tour
      (values (make-array 0 :element-type '(integer 0 #.most-positive-fixnum))
              (make-array 0 :element-type '(integer 0 #.most-positive-fixnum))
              (make-array 0 :element-type '(integer 0 #.most-positive-fixnum)))))
  (let* ((n (length tree))
         (tour (make-array (- (* 2 n) 1)
                           :element-type '(integer 0 #.most-positive-fixnum)
                           :initial-element 0))
         (pre (make-array n :element-type '(integer 0 #.most-positive-fixnum)))
         (post (make-array n :element-type '(integer 0 #.most-positive-fixnum)))
         (index 0))
    (declare ((integer 0 #.most-positive-fixnum) index))
    (labels ((dfs (v parent)
               (setf (aref pre v) index)
               (dolist (node (aref tree v))
                 (let ((child (funcall key node)))
                   (declare ((integer 0 #.most-positive-fixnum) child))
                   (unless (= child parent)
                     (incf index)
                     (setf (aref tour index) child)
                     (dfs child v)
                     (incf index)
                     (setf (aref tour index) v))))
               (setf (aref post v) index)))
      (unless (zerop n)
        (dfs root -1))
      (values tour pre post))))
