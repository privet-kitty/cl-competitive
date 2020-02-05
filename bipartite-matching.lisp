;;;
;;; Maximum bipartite matching (Ford-Fulkerson)
;;; (better to use Hopcroft-Karp instead)
;;;
;;; Referene:
;;; Akiha, Iwata, Kitagawa. Programming Contest Challenge Book (Japanese)
;;;

(declaim (ftype (function * (values (or null (simple-array fixnum (*)))
                                    (integer 0 #.most-positive-fixnum)
                                    &optional))
                find-matching))
(defun find-matching (graph)
  "Takes an adjacency list and returns the maximum bipartite matching. Note that
this function doesn't check if GRAPH is truly bipartite."
  (declare (optimize (speed 3))
           ((simple-array list (*)) graph))
  (let* ((n (length graph))
         (visited (make-array n :element-type 'bit :initial-element 0))
         (matching (make-array n :element-type 'fixnum :initial-element -1))
         (count 0))
    (declare ((integer 0 #.most-positive-fixnum) count))
    (labels ((%match (vertex) ; Returns T if VERTEX is matched.
               (setf (aref visited vertex) 1)
               (dolist (candidate (aref graph vertex))
                 (let ((partner-of-candidate (aref matching candidate)))
                   (when (or (= -1 partner-of-candidate)
                             (and (zerop (aref visited partner-of-candidate))
                                  (%match partner-of-candidate)))
                     (setf (aref matching vertex) candidate
                           (aref matching candidate) vertex)
                     (return t))))))
      (dotimes (v n (values matching count))
        (when (= -1 (aref matching v))
          (fill visited 0)
          (when (%match v)
            (incf count)))))))
