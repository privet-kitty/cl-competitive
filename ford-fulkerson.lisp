;;;
;;; Ford-Fulkerson
;;;

(setf *print-circle* t)

(defstruct (edge (:constructor %make-edge))
  (to nil :type fixnum)
  (capacity 0 :type fixnum)
  (flow 0 :type fixnum)
  (reversed nil :type (or null edge)))

(defun push-edge (from-idx to-idx capacity graph &key bidirectional)
  "FROM-IDX, TO-IDX := index of vertex
GRAPH := vector of lists of all the edges that goes from each vertex

If BIDIRECTIONAL is true, PUSH-EDGE adds the reversed edge of the
same capacity."
  (declare ((simple-array list (*)) graph))
  (let* ((dep (%make-edge :to to-idx :capacity capacity))
         (ret (%make-edge :to from-idx
                          :capacity (if bidirectional capacity 0)
                          :reversed dep)))
    (setf (edge-reversed dep) ret)
    (push dep (aref graph from-idx))
    (push ret (aref graph to-idx))))

(defun %find-flow (src-idx dest-idx graph max-flow checked)
  "DFS"
  (declare ((integer 0 #.most-positive-fixnum) src-idx dest-idx max-flow)
           (simple-bit-vector checked)
           ((simple-array list (*)) graph))
  (setf (aref checked src-idx) 1)
  (if (= src-idx dest-idx)
      max-flow
      (dolist (edge (aref graph src-idx) 0)
        (when (and (zerop (aref checked (edge-to edge)))
                   (< (edge-flow edge) (edge-capacity edge)))
          (let ((flow (%find-flow (edge-to edge)
                                  dest-idx
                                  graph
                                  (min max-flow (- (edge-capacity edge) (edge-flow edge)))
                                  checked)))
            (declare ((integer 0 #.most-positive-fixnum) flow))
            (unless (zerop flow)
              (incf (edge-flow edge) flow)
              (incf (edge-capacity (edge-reversed edge)) flow)
              (return flow)))))))

(defun max-flow (src-idx dest-idx graph)
  (declare ((simple-array list (*)) graph))
  (let ((checked (make-array (length graph) :element-type 'bit :initial-element 0)))
    (loop for incr-flow of-type (integer 0 #.most-positive-fixnum)
             = (%find-flow src-idx dest-idx graph most-positive-fixnum checked)
          until (zerop incr-flow)
          sum incr-flow of-type (integer 0 #.most-positive-fixnum)
          do (fill checked 0))))
