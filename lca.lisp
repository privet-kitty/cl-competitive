;;;
;;; Lowest common anscestor of tree (binary lifting)
;;; build: O(nlog(n))
;;; query: O(log(n))
;;;

;; PAY ATTENTION TO THE STACK SIZE! BUILD-LCA-TABLE does DFS.

(deftype lca-vertex-number () '(signed-byte 32))

(defstruct (lca-table
            (:constructor make-lca-table
                (size
                 &aux
                 (depths (make-array size :element-type 'lca-vertex-number))
                 ;; requires 1 + log_2{size-1}
                 (parents (make-array (list size (+ 1 (integer-length (- size 2))))
                                      :element-type 'lca-vertex-number))))
            (:conc-name lca-))
  (depths nil :type (simple-array lca-vertex-number (*)))
  (parents nil :type (simple-array lca-vertex-number (* *))))

(defun build-lca-table (root graph &key (key #'identity))
  (declare (optimize (speed 3))
           ((simple-array list (*)) graph)
           (function key))
  (let* ((size (length graph))
         (lca-table (make-lca-table size))
         (depths (lca-depths lca-table))
         (parents (lca-parents lca-table))
         (max-log-v (array-dimension parents 1)))
    (labels ((dfs (v prev-v depth)
               (declare (lca-vertex-number v prev-v))
               (setf (aref depths v) depth)
               (setf (aref parents v 0) prev-v)
               (dolist (node (aref graph v))
                 (let ((dest (funcall key node)))
                   (declare (lca-vertex-number dest))
                   (unless (= dest prev-v)
                     (dfs dest v (+ 1 depth)))))))
      (dfs root -1 0)
      (dotimes (k (- max-log-v 1))
        (dotimes (v size)
          (if (= -1 (aref parents v k))
              (setf (aref parents v (+ k 1)) -1)
              (setf (aref parents v (+ k 1)) (aref parents (aref parents v k) k)))))
      lca-table)))

(defun get-lca (u v lca-table)
  (declare (optimize (speed 3))
           (lca-vertex-number u v))
  (let* ((depths (lca-depths lca-table))
         (parents (lca-parents lca-table))
         (max-log-v (array-dimension parents 1)))
    ;; Ensures depth[u] <= depth[v]
    (when (> (aref depths u) (aref depths v)) (rotatef u v))
    (dotimes (k max-log-v)
      (when (logbitp k (- (aref depths v) (aref depths u)))
        (setf v (aref parents v k))))
    (if (= u v)
        u
        (loop for k from (- max-log-v 1) downto 0
              unless (= (aref parents u k) (aref parents v k))
              do (setf u (aref parents u k)
                       v (aref parents v k))
              finally (return (aref parents u 0))))))

(defun distance-on-tree (u v lca-table)
  (declare (optimize (speed 3)))
  (let ((depths (lca-depths lca-table))
        (lca (get-lca u v lca-table)))
    (+ (- (aref depths u) (aref depths lca))
       (- (aref depths v) (aref depths lca)))))
