(declaim (inline derive-rooted-tree)
         (ftype (function * (values (simple-array list (*)) &optional)) derive-rooted-tree))
(defun derive-rooted-tree (tree root)
  "Derives a directed rooted tree from an undirected TREE.

TREE := vector of adjacency lists"
  (declare (vector tree)
           ((integer 0 #.most-positive-fixnum) root))
  (let ((result (make-array (length tree) :element-type 'list :initial-element nil)))
    (labels ((recur (v parent)
               (let (adj-list)
                 (dolist (child (aref tree v))
                   (declare ((integer 0 #.most-positive-fixnum) child))
                   (unless (= child parent)
                     (push child adj-list)
                     (recur child v)))
                 (setf (aref result v) adj-list))))
      (recur root -1))
    result))
