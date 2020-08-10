;;;
;;; Merge sort
;;;

(declaim (inline %merge))
(defun %merge (l mid r source-vec dest-vec order key)
  (declare ((integer 0 #.array-total-size-limit) l mid r)
           (function order key))
  (loop with pos1 = l
        with pos2 = mid
        for dest from l
        when (= pos1 mid)
        do (loop for pos2 from pos2 below r
                 for dest from dest
                 do (setf (aref dest-vec dest)
                          (aref source-vec pos2))
                 finally (return-from %merge t))
        when (= pos2 r)
        do (loop for pos1 from pos1 below mid
                 for dest from dest
                 do (setf (aref dest-vec dest)
                          (aref source-vec pos1))
                 finally (return-from %merge t))
        do (if (funcall order
                        (funcall key (aref source-vec pos1))
                        (funcall key (aref source-vec pos2)))
               (setf (aref dest-vec dest) (aref source-vec pos1)
                     pos1 (1+ pos1))
               (setf (aref dest-vec dest) (aref source-vec pos2)
                     pos2 (1+ pos2)))))

(declaim (inline %insertion-sort!))
(defun %insertion-sort! (vector order l r key)
  (declare (function order key)
           ((integer 0 #.array-total-size-limit) l r))
  (loop for end from (+ l 1) below r
        do (loop for i from end above l
                 while (funcall order
                                (funcall key (aref vector i))
                                (funcall key (aref vector (- i 1))))
                 do (rotatef (aref vector (- i 1)) (aref vector i)))
        finally (return vector)))

;; NOTE: This merge sort is slow on SBCL version earlier than 1.5.0 as the type
;; propagation of MAKE-ARRAY doesn't work. array-element-type.lisp is required to
;; enable the optimization.

;; TODO: Peephole optimization of SBCL is not sufficient to optimize empty KEY
;; function. Defining deftransform will work.
(declaim (inline merge-sort!))
(defun merge-sort! (vector order &key (start 0) end (key #'identity))
  "Destructively sorts VECTOR. (You can rely on the side effect.)

ORDER := strict order"
  (declare (vector vector)
           (function order key))
  (let* ((end (or end (length vector)))
         ;; TODO: avoid to allocate excessive size
         (buffer (make-array end :element-type (array-element-type vector))))
    (declare ((integer 0 #.array-total-size-limit) start end))
    (labels ((recur (l r merge-to-vector-p)
               (declare ((mod #.array-total-size-limit) l r))
               (if (and (<= (- r l) 32) merge-to-vector-p)
                   (%insertion-sort! vector order l r key)
                   (let ((mid (floor (+ l r) 2)))
                     (recur l mid (not merge-to-vector-p))
                     (recur mid r (not merge-to-vector-p))
                     (if merge-to-vector-p
                         (%merge l mid r buffer vector order key)
                         (%merge l mid r vector buffer order key))))))
      (recur start end t)
      vector)))
