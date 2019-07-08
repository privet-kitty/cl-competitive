;; PAY ATTENTION TO THE STACK SIZE!
(declaim (inline make-dfs-order))
(defun make-dfs-order (tree &key (key #'identity) (root 0))
  "Maps each vertex of (rooted) TREE to the first index obtained by Euler tour.

KEY is called to each node of adjacency list and must return the index number of
the vertex.

TREE := vector of adjacency lists"
  (declare (vector tree)
           (function key)
           ((integer 0 #.most-positive-fixnum) root))
  (let* ((n (length tree))
         (res (make-array n :element-type '(integer 0 #.most-positive-fixnum)
                            :initial-element 0))
         (index 0))
    (declare ((integer 0 #.most-positive-fixnum) index))
    (labels ((dfs (v parent)
               (dolist (node (aref tree v))
                 (let ((neighbor (funcall key node)))
                   (declare ((integer 0 #.most-positive-fixnum) neighbor))
                   (unless (= neighbor parent)
                     (incf index)
                     (setf (aref res neighbor) index)
                     (dfs neighbor v)
                     (incf index))))))
      (dfs root -1)
      res)))
