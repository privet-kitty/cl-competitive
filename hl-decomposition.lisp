;;;
;;; Heavy-Light decomposition (for commutative operation)
;;;

(defstruct (hl-decomposition (:constructor %make-hl-decomposition
                                 (graph sizes parents preords heads))
                             (:conc-name %hld-))
  (graph nil :type (simple-array list (*)))
  (sizes nil :type (simple-array fixnum (*)))
  (parents nil :type (simple-array fixnum (*)))
  ;; numbering of vertices in pre-order
  (preords nil :type (simple-array fixnum (*)))
  ;; heads of heavy paths
  (heads nil :type (simple-array fixnum (*))))

(defun make-hl-decomposition (graph &key (roots 0) (key #'identity))
  (declare (function key)
           (vector graph))
  (let* ((n (length graph))
         (original-graph graph)
         (graph (make-array n :element-type 'list :initial-element nil))
         (tails (make-array n :element-type 'list :initial-element nil))
         (sizes (make-array n :element-type 'fixnum :initial-element 1))
         (parents (make-array n :element-type 'fixnum))
         (heads (make-array n :element-type 'fixnum))
         (preords (make-array n :element-type 'fixnum))
         (index 0)
         (roots (if (listp roots) roots (list roots))))
    (declare ((integer 0 #.array-total-size-limit) index))
    (labels ((push-back (v child)
               (if (aref tails v)
                   (setf (cdr (aref tails v)) (list child)
                         (aref tails v) (cdr (aref tails v)))
                   (setf (aref graph v) (list child)
                         (aref tails v) (aref graph v))))
             (push-front (v child)
               (push child (aref graph v))
               (unless (aref tails v)
                 (setf (aref tails v) (aref graph v))))
             ;; move the largest subtrees to the heads of adjacency lists
             (dfs-size (v parent)
               (let ((size 0))
                 (declare (fixnum size))
                 (dolist (edge (aref original-graph v))
                   (let ((child (funcall key edge)))
                     (declare ((integer 0 #.array-total-size-limit) child))
                     (if (= child parent)
                         (push-back v child)
                         (progn
                           (dfs-size child v)
                           (incf (aref sizes v) (aref sizes child))
                           (if (> (aref sizes child) size)
                               (push-front v child)
                               (push-back v child))
                           (setq size (max size (aref sizes child)))))))))
             ;; numbers all vertices in pre-order
             (dfs-hld (v parent)
               (setf (aref parents v) parent
                     (aref preords v) index
                     index (+ index 1))
               (dolist (child (aref graph v))
                 (declare ((integer 0 #.array-total-size-limit) child))
                 (unless (= child parent)
                   (setf (aref heads child)
                         (if (eql child (car (aref graph v)))
                             (aref heads v)
                             child))
                   (dfs-hld child v)))))
      (dolist (root roots)
        (dfs-size root -1)
        (setf (aref heads root) root)
        (dfs-hld root -1))
      (%make-hl-decomposition graph sizes parents preords heads))))

(define-condition two-vertices-disconnected-error (error)
  ((hld :initarg :hld :accessor two-vertices-disconnected-error-hld)
   (vertex1 :initarg :vertex1 :accessor two-vertices-disconnected-error-vertex1)
   (vertex2 :initarg :vertex2 :accessor two-vertices-disconnected-error-vertex2))
  (:report
   (lambda (c s)
     (format s "~W and ~W are disconnected on HLD ~W"
             (two-vertices-disconnected-error-vertex1 c)
             (two-vertices-disconnected-error-vertex2 c)
             (two-vertices-disconnected-error-hld c)))))

(declaim (inline hld-map-path))
(defun hld-map-path (hld vertex1 vertex2 function)
  "Maps all heavy paths in the path between given vertices. FUNCTION takes two
arguments which are both ends of a heavy path represented in pre-order
numbering. Note that they are **closed** intervals."
  (let ((u vertex1)
        (v vertex2)
        (preords (%hld-preords hld))
        (heads (%hld-heads hld))
        (parents (%hld-parents hld)))
    (loop (when (> (aref preords u) (aref preords v))
            (rotatef u v))
          (funcall function
                   (max (aref preords (aref heads v))
                        (aref preords u))
                   (aref preords v))
          (when (= (aref heads u) (aref heads v))
            (return))
          (setq v (aref parents (aref heads v)))
          (when (= -1 v)
            (error 'two-vertices-disconnected-error
                   :vertex1 vertex1 :vertex2 vertex2 :hld hld)))))

(declaim (inline hld-get-lca))
(defun hld-get-lca (hld vertex1 vertex2)
  "Returns the lowest common anscestor of VERTEX1 and VERTEX2."
  (let ((u vertex1)
        (v vertex2)
        (preords (%hld-preords hld))
        (heads (%hld-heads hld))
        (parents (%hld-parents hld)))
    (loop (when (> (aref preords u) (aref preords v))
            (rotatef u v))
          (when (= (aref heads u) (aref heads v))
            (return u))
          (setq v (aref parents (aref heads v)))
          (when (= -1 v)
            (error 'two-vertices-disconnected-error
                   :vertex1 vertex1 :vertex2 vertex2 :hld hld)))))
