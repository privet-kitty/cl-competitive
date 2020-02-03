;; PAY ATTENTION TO THE STACK SIZE!
(declaim (inline make-dfs-order))
(defun make-dfs-order (tree &key (key #'identity) (root 0))
  "Maps each vertex of (rooted) TREE to the index obtained by Euler tour. This
function returns two vector: PRE, POST; PRE[i] is the first index of the vertex
i on Euler tour; POST[i] is the last index.

KEY is called to each node of adjacency list and must return the index number of
the vertex.

Note that this function doesn't check if TREE is really a tree. The behaviour
for a non-tree is undefined.

TREE := vector of adjacency lists"
  (declare (vector tree)
           (function key)
           ((integer 0 #.most-positive-fixnum) root))
  (let* ((n (length tree))
         (pre (make-array n :element-type '(integer 0 #.most-positive-fixnum)))
         (post (make-array n :element-type '(integer 0 #.most-positive-fixnum)))
         (index 0))
    (declare ((integer 0 #.most-positive-fixnum) index))
    (labels ((dfs (v parent)
               (setf (aref pre v) index)
               (incf index)
               (dolist (node (aref tree v))
                 (let ((neighbor (funcall key node)))
                   (declare ((integer 0 #.most-positive-fixnum) neighbor))
                   (unless (= neighbor parent)
                     (dfs neighbor v))))
               (setf (aref post v) index)
               (incf index)))
      (unless (zerop n)
        (dfs root -1))
      (values pre post))))
