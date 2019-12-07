;;;
;;; Diameter of tree
;;;

(declaim (inline find-diameter))
(defun find-diameter (graph)
  "Finds a diameter of a tree. Returns three values: the length of the diameter
and its two ends."
  (declare ((array list (*)) graph))
  (let ((end 0)
        (max-depth 0))
    (labels ((dfs (v parent depth)
               (declare ((integer 0 #.most-positive-fixnum) v depth)
                        ((integer -1 #.most-positive-fixnum) parent))
               (when (> depth max-depth)
                 (setq max-depth depth
                       end v))
               (dolist (child (aref graph v))
                 (declare ((integer 0 #.most-positive-fixnum) child))
                 (unless (= child parent)
                   (dfs child v (+ depth 1))))))
      (dfs 0 -1 0)
      (let ((end1 end))
        (setq max-depth 0)
        (dfs end1 -1 0)
        (values max-depth end1 end)))))
