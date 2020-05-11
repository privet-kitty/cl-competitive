;;;
;;; Topological sort
;;; Reference:
;;; https://en.wikipedia.org/wiki/Topological_sorting#Depth-first_search
;;;

(define-condition cycle-detected-error (error)
  ((graph :initarg :graph :reader cycle-detected-error-graph)
   (vertex :initarg :vertex :reader cycle-detected-error-vertex))
  (:report
   (lambda (condition stream)
     (format stream "Detected a cycle containing ~A in ~A."
             (cycle-detected-error-vertex condition)
             (cycle-detected-error-graph condition)))))

(declaim (ftype (function * (values (simple-array (integer 0 #.most-positive-fixnum) (*)) &optional))
                topological-sort)
         (inline topological-sort))
(defun topological-sort (graph)
  "Returns a vector of all the vertices sorted in topological order. This
function signals CYCLE-DETECTED-ERROR when it detects a cycle.

GRAPH := vector of adjacency lists."
  (declare (vector graph))
  (let* ((n (length graph))
         (tmp-marked (make-array n :element-type 'bit :initial-element 0))
         (marked (make-array n :element-type 'bit :initial-element 0))
         (result (make-array n :element-type '(integer 0 #.most-positive-fixnum)))
         (index (- n 1)))
    ;; I don't introduce dynamic-extent here just because the compiler notes in
    ;; SBCL are a bit annoying and it is rarely effective.
    (declare (fixnum index))
    (labels ((visit (v)
               (when (= 0 (aref marked v))
                 (when (= 1 (aref tmp-marked v))
                   (error 'cycle-detected-error :graph graph :vertex v))
                 (setf (aref tmp-marked v) 1)
                 (dolist (next (aref graph v))
                   (visit next))
                 (setf (aref marked v) 1
                       (aref result index) v)
                 (decf index))))
      (dotimes (v n result)
        (when (= 0 (aref marked v))
          (visit v))))))
