;;;
;;; Strongly connected components of directed graph, 2-SAT
;;;

(defstruct (scc (:constructor %make-scc (graph components sizes count)))
  (graph nil :type vector)
  ;; components[i] := strongly connected component of the i-th vertex
  (components nil :type (simple-array (integer 0 #.most-positive-fixnum) (*)))
  ;; sizes[k] := size of the k-th strongly connected component
  (sizes nil :type (simple-array (integer 0 #.most-positive-fixnum) (*)))
  ;; the total number of strongly connected components
  (count 0 :type (integer 0 #.most-positive-fixnum)))

;; Reference: http://www.prefield.com/algorithm/graph/strongly_connected_components.html
;; This is faster than well-known Kosaraju's algorithm because it doesn't
;; generate a reversed graph. I don't know the first person who referred to this
;; algorithm (tmaehara's invention?)
(defun make-scc (graph)
  (declare (optimize (speed 3))
           (vector graph))
  (let* ((n (length graph))
         (ord 0)
         (ords (make-array n :element-type 'fixnum :initial-element -1)) ; in-order
         ;; store the lowest in-order number as the representative element of a
         ;; strongly connected component
         (lowests (make-array n :element-type 'fixnum))
         (components (make-array n :element-type '(integer 0 #.most-positive-fixnum)))
         (comp-index 0) ; index number of component
         (sizes (make-array n :element-type '(integer 0 #.most-positive-fixnum)
                              :initial-element 0))
         (stack (make-array n :element-type '(integer 0 #.most-positive-fixnum)))
         (end 0) ; stack pointer
         (in-stack (make-array n :element-type 'bit :initial-element 0)))
    (declare ((integer 0 #.most-positive-fixnum) ord end comp-index))
    (labels ((%push (v)
               (setf (aref stack end) v
                     (aref in-stack v) 1)
               (incf end))
             (%pop ()
               (decf end)
               (let ((v (aref stack end)))
                 (setf (aref in-stack v) 0)
                 v))
             (visit (v)
               (setf (aref ords v) ord
                     (aref lowests v) ord)
               (incf ord)
               (%push v)
               (dolist (next (aref graph v))
                 (cond ((= -1 (aref ords next))
                        (visit next)
                        (setf (aref lowests v)
                              (min (aref lowests v) (aref lowests next))))
                       ((= 1 (aref in-stack next))
                        (setf (aref lowests v)
                              (min (aref lowests v) (aref ords next))))))
               (when (= (aref lowests v) (aref ords v))
                 (loop for size of-type (integer 0 #.most-positive-fixnum) from 1
                       for w = (%pop)
                       do (setf (aref components w) comp-index)
                       until (= v w)
                       finally (setf (aref sizes comp-index) size)
                               (incf comp-index)))))
      (dotimes (v n)
        (when (= -1 (aref ords v))
          (visit v)))
      ;; Reverse the order of strongly connected components, because now
      ;; everything is in the reversed topological order
      (dotimes (v n)
        (setf (aref components v)
              (- comp-index (aref components v) 1)))
      (dotimes (i (ash comp-index -1))
        (rotatef (aref sizes i) (aref sizes (- comp-index i 1))))
      (%make-scc graph components sizes comp-index))))


(declaim (ftype (function * (values (simple-array t (*)) &optional))
                make-condensed-graph))
(defun make-condensed-graph (scc)
  "Does graph condensation.

This function is non-destructive. The resultant graph doesn't contain self-loops
even if the given graph does."
  (declare (optimize (speed 3)))
  (let* ((graph (scc-graph scc))
         (n (length graph))
         (comp-n (scc-count scc))
         (components (scc-components scc))
         (condensed (make-array comp-n :element-type t)))
    (dotimes (i comp-n)
      (setf (aref condensed i) (make-hash-table :test #'eql)))
    (dotimes (i n)
      (let ((i-comp (aref components i)))
        (dolist (neighbor (aref graph i))
          (let ((neighbor-comp (aref components neighbor)))
            (unless (= i-comp neighbor-comp)
              (setf (gethash neighbor-comp (aref condensed i-comp)) t))))))
    (dotimes (i comp-n)
      (setf (aref condensed i)
            (loop for x being each hash-key of (aref condensed i) collect x)))
    condensed))

;;;
;;; 2-SAT
;;;

(defstruct (2sat (:constructor make-2sat
                     (size
                      &aux
                      (graph (make-array (* 2 size) :element-type 'list :initial-element nil)))))
  (size 0 :type (integer 0 #.most-positive-fixnum))
  (graph nil :type (simple-array list (*)))
  (scc nil :type (or null scc)))

(declaim (inline negate))
(defun negate (p)
  (- -1 p))

(declaim (inline %add-implication))
(defun %add-implication (2sat p q)
  (declare (fixnum p q))
  (let ((size (2sat-size 2sat))
        (graph (2sat-graph 2sat)))
    (when (< p 0)
      (setq p (+ size (- -1 p))))
    (when (< q 0)
      (setq q (+ size (- -1 q))))
    (push q (aref graph p))
    2sat))

(defun add-implication (2sat p q)
  (declare (fixnum p q))
  (%add-implication 2sat p q)
  (%add-implication 2sat (negate q) (negate p))
  2sat)

(declaim (inline add-disjunction))
(defun add-disjunction (2sat p q)
  "Adds `P or Q' to 2SAT."
  (declare (fixnum p q))
  (%add-implication 2sat (negate p) q)
  (%add-implication 2sat (negate q) p)
  2sat)

(declaim (inline 2sat-solve))
(defun 2sat-solve (2sat)
  "Solves 2-SAT and returns a simple bit vector expressing the boolean of each
variable if it is feasible, otherwise returns NIL."
  (let* ((size (2sat-size 2sat))
         (graph (2sat-graph 2sat))
         (scc (make-scc graph))
         (components (scc-components scc))
         (result (make-array size :element-type 'bit :initial-element 0)))
    (setf (2sat-scc 2sat) scc)
    (loop for v below size
          for v-comp = (aref components v)
          for neg-comp = (aref components (+ v size))
          do (cond ((> v-comp neg-comp)
                    (setf (sbit result v) 1))
                   ((= v-comp neg-comp)
                    (return-from 2sat-solve nil))))
    result))

