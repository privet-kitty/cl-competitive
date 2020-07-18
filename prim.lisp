;;;
;;; MST (Prim's algorithm)
;;; (Poorly implemented and obsolete. I leave it just for my reference.)
;;;

(defstruct edge
  (to nil :type fixnum)
  (cost 0 :type fixnum))

(defun add-edge (from-idx to-idx cost graph &key (bidirectional nil))
  "FROM-IDX, TO-IDX := index of vertex
GRAPH := vector of list of all the edges that goes from the vertex"
  (push (make-edge :to to-idx :cost cost) (aref graph from-idx))
  (when bidirectional
    (push (make-edge :to from-idx :cost cost) (aref graph to-idx))))

;; binary heap (for priority queue)
(defstruct (heap (:constructor make-heap
                            (size
                             &key test (element-type t)
                             &aux (data (make-array (1+ size) :element-type element-type)))))
  (data #() :type (simple-array edge (*)) :read-only t)
  (test #'< :type function :read-only t)
  (next-position 1 :type (integer 1 #.most-positive-fixnum)))

(defun heap-push (obj heap)
  (symbol-macrolet ((data (heap-data heap))
                    (test (heap-test heap))
                    (next-position (heap-next-position heap)))
    (labels ((update (pos)
               (unless (= pos 1)
                 (let ((parent-pos (floor pos 2)))
                   (when (funcall test (aref data pos) (aref data parent-pos))
                     (rotatef (aref data pos) (aref data parent-pos))
                     (update parent-pos))))))
      (setf (aref data next-position) obj)
      (update next-position)
      (incf next-position)
      heap)))

(defun heap-pop (heap &optional (error t) null-value)
  (symbol-macrolet ((data (heap-data heap))
                    (test (heap-test heap))
                    (next-position (heap-next-position heap)))
    (labels ((update (pos)
               (declare ((integer 1 #.most-positive-fixnum) pos))
               (let* ((child-pos1 (+ pos pos))
                      (child-pos2 (1+ child-pos1)))
                 (when (<= child-pos1 next-position)
                   (if (<= child-pos2 next-position)
                       (if (funcall test (aref data child-pos1) (aref data child-pos2))
                           (unless (funcall test (aref data pos) (aref data child-pos1))
                             (rotatef (aref data pos) (aref data child-pos1))
                             (update child-pos1))
                           (unless (funcall test (aref data pos) (aref data child-pos2))
                             (rotatef (aref data pos) (aref data child-pos2))
                             (update child-pos2)))
                       (unless (funcall test (aref data pos) (aref data child-pos1))
                         (rotatef (aref data pos) (aref data child-pos1))))))))
      (if (= next-position 1)
          (if error
              (error "No element in heap")
              null-value)
          (prog1 (aref data 1)
            (decf next-position)
            (setf (aref data 1) (aref data next-position))
            (update 1))))))

;; body
(defun find-mst (graph &key queue-size)
  "Prim's algorithm"
  (declare ((simple-array list (*)) graph))
  (let ((pqueue (make-heap (or queue-size
                               (loop for edges across graph
                                     sum (length edges) of-type fixnum))
                           :test (lambda (e1 e2) (< (edge-cost e1) (edge-cost e2)))))
        (added (make-array (length graph) :element-type 'boolean :initial-element nil))
        (total-cost 0))
    (declare (fixnum total-cost))
    (heap-push (make-edge :to 0 :cost 0) pqueue)
    (loop for edge = (heap-pop pqueue nil)
          while edge
          do (unless (aref added (edge-to edge))
               (setf (aref added (edge-to edge)) t)
               (incf total-cost (edge-cost edge))
               (dolist (next-edge (aref graph (edge-to edge)))
                 (heap-push next-edge pqueue))))
    total-cost))

;; Test
;; (let ((graph (make-array 7 :element-type 'list :initial-element nil)))
;;   (add-edge 0 1 7 graph :bidirectional t)
;;   (add-edge 0 3 5 graph :bidirectional t)
;;   (add-edge 1 3 9 graph :bidirectional t)
;;   (add-edge 1 2 8 graph :bidirectional t)
;;   (add-edge 2 4 5 graph :bidirectional t)
;;   (add-edge 1 4 7 graph :bidirectional t)
;;   (add-edge 3 4 15 graph :bidirectional t)
;;   (add-edge 3 5 6 graph :bidirectional t)
;;   (add-edge 4 5 8 graph :bidirectional t)
;;   (add-edge 4 6 9 graph :bidirectional t)
;;   (add-edge 5 6 11 graph :bidirectional t)
;;   (assert (= 39 (find-mst graph))))
