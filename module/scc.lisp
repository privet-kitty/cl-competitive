;;;
;;; Strongly connected components of directed graph
;;;

(defpackage :cp/scc
  (:use :cl)
  (:export #:scc #:scc-graph #:scc-components #:scc-sizes #:scc-count
           #:scc-p #:make-scc #:make-condensed-graph))
(in-package :cp/scc)

(defstruct (scc (:constructor %make-scc (graph components sizes count))
                (:copier nil)
                (:predicate nil))
  (graph nil :type vector)
  ;; components[i] := strongly connected component of the i-th vertex
  (components nil :type (simple-array (integer 0 #.most-positive-fixnum) (*)))
  ;; sizes[k] := size of the k-th strongly connected component
  (sizes nil :type (simple-array (integer 0 #.most-positive-fixnum) (*)))
  ;; the total number of strongly connected components
  (count 0 :type (integer 0 #.most-positive-fixnum)))

;; Tarjan's algorithm
;; Reference: http://www.prefield.com/algorithm/graph/strongly_connected_components.html
;; (Kosaraju's algorithm is put in the test file)
(defun make-scc (graph)
  (declare (optimize (speed 3))
           (vector graph))
  (let* ((n (length graph))
         (ord 0)
         (ords (make-array n :element-type 'fixnum :initial-element -1)) ; in-order
         ;; store the lowest in-order number as the representative element of a
         ;; strongly connected component
         (lowlinks (make-array n :element-type 'fixnum))
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
                     (aref lowlinks v) ord)
               (incf ord)
               (%push v)
               (dolist (next (aref graph v))
                 (cond ((= -1 (aref ords next))
                        (visit next)
                        (setf (aref lowlinks v)
                              (min (aref lowlinks v) (aref lowlinks next))))
                       ((= 1 (aref in-stack next))
                        (setf (aref lowlinks v)
                              (min (aref lowlinks v) (aref ords next))))))
               (when (= (aref lowlinks v) (aref ords v))
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
      (%make-scc graph components (adjust-array sizes comp-index) comp-index))))

;; FIXME: Constant factor of this implementation is too large. Can we avoid
;; hash-table?
(declaim (ftype (function * (values (simple-array t (*)) &optional))
                make-condensed-graph))
(defun make-condensed-graph (scc)
  "Does graph condensation. This function is non-destructive."
  (declare (optimize (speed 3)))
  (let* ((graph (scc-graph scc))
         (n (length graph))
         (comp-n (scc-count scc))
         (components (scc-components scc))
         (condensed (make-array comp-n :element-type t)))
    (dotimes (i comp-n)
      ;; Resorting to EQ is substandard, though I use it here for efficiency.
      (setf (aref condensed i) (make-hash-table :test #'eq)))
    (dotimes (i n)
      (let ((i-comp (aref components i)))
        (dolist (neighbor (aref graph i))
          (let ((neighbor-comp (aref components neighbor)))
            (unless (= i-comp neighbor-comp)
              (setf (gethash neighbor-comp (aref condensed i-comp)) t))))))
    (dotimes (i comp-n)
      (setf (aref condensed i)
            (loop for x being each hash-key of (aref condensed i)
                  collect x)))
    condensed))
