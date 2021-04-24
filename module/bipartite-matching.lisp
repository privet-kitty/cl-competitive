;;;
;;; Maximum bipartite matching (Ford-Fulkerson)
;;; (Not fast. Better to use Hopcroft-Karp if you need speed)
;;;
;;; Referene:
;;; Akiha, Iwata, Kitagawa. Programming Contest Challenge Book (Japanese)
;;;

(defpackage :cp/bipartite-matching
  (:use :cl)
  (:export #:find-matching))
(in-package :cp/bipartite-matching)

(declaim (ftype (function * (values (simple-array fixnum (*))
                                    (mod #.array-dimension-limit)
                                    &optional))
                find-matching))
(defun find-matching (graph)
  "Returns a maximum bipartite matching of GRAPH. Note that this function
doesn't check if GRAPH is actually bipartite.

GRAPH := vector of adjacency lists"
  (declare (optimize (speed 3))
           (vector graph))
  (let* ((n (length graph))
         (visited (make-array n :element-type 'bit :initial-element 0))
         (matching (make-array n :element-type 'fixnum :initial-element -1))
         (count 0))
    (declare ((integer 0 #.most-positive-fixnum) count))
    (labels ((match (vertex) ; Returns T if VERTEX is matched.
               (setf (aref visited vertex) 1)
               (dolist (candidate (aref graph vertex))
                 (let ((partner-of-candidate (aref matching candidate)))
                   (when (or (= -1 partner-of-candidate)
                             (and (zerop (aref visited partner-of-candidate))
                                  (match partner-of-candidate)))
                     (setf (aref matching vertex) candidate
                           (aref matching candidate) vertex)
                     (return t))))))
      (dotimes (v n)
        (when (= -1 (aref matching v))
          (fill visited 0)
          (when (match v)
            (incf count))))
      (values matching count))))
