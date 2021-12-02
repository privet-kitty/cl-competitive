(defpackage :cp/bipartite-matching
  (:use :cl)
  (:export #:find-matching)
  (:documentation "Provides Ford-Fulkerson algorithm for bipartite
matching. (Not fast. You should use Hopcroft-Karp instead.)

Reference:
Akiha, Iwata, Kitagawa. Programming Contest Challenge Book. (Japanese)"))
(in-package :cp/bipartite-matching)

(declaim (ftype (function * (values (simple-array fixnum (*))
                                    (mod #.array-dimension-limit)
                                    &optional))
                find-matching))
(defun find-matching (graph)
  "Returns a maximum bipartite matching on GRAPH. Note that this function
doesn't check if GRAPH is actually bipartite.

GRAPH := vector of adjacency lists"
  (declare (optimize (speed 3))
           (vector graph))
  (let* ((n (length graph))
         (visited (make-array n :element-type 'bit :initial-element 0))
         (matching (make-array n :element-type 'fixnum :initial-element -1))
         (count 0))
    (declare ((mod #.array-dimension-limit) count))
    (labels ((match (vertex) ; Returns T if VERTEX is matched.
               (setf (aref visited vertex) 1)
               (dolist (neighbor (aref graph vertex))
                 (let ((partner-of-neighbor (aref matching neighbor)))
                   (when (or (= -1 partner-of-neighbor)
                             (and (zerop (aref visited partner-of-neighbor))
                                  (match partner-of-neighbor)))
                     (setf (aref matching vertex) neighbor
                           (aref matching neighbor) vertex)
                     (return t))))))
      (dotimes (v n)
        (when (= -1 (aref matching v))
          (fill visited 0)
          (when (match v)
            (incf count))))
      (values matching count))))
