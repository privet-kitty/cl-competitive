;;;
;;; Maximum bipartite matching by Ford-Fulkerson
;;;

(defun find-matching (graph)
  "Takes an adjacency list and returns the maximal bipartite matching. Note that
this function doesn't check if GRAPH is truly bipartite."
  (declare ((simple-array list (*)) graph))
  (let* ((n (length graph))
         (visited (make-array n :element-type 'bit :initial-element 0))
         (matching (make-array n :element-type 'fixnum :initial-element -1))
         (res 0))
    (declare ((integer 0 #.most-positive-fixnum) res))
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
      (dotimes (v n (values matching res))
        (when (= -1 (aref matching v))
          (fill visited 0)
          (when (%match v)
            (incf res)))))))

;; test
(defun test-bipartite-matching ()
  (let* ((graph (make-array 9
                            :element-type 'list
                            :initial-contents '((6) (5 6 7 8) (6) (6) (5) (1 4) (0 1 2 3) (1) (1))))
         (matching (find-matching graph)))
    (loop for i below 9
          do (assert (or (= (aref matching i) -1)
                         (= i (aref matching (aref matching i))))))
    (assert (= 6 (count -1 matching :test-not #'=)))))
