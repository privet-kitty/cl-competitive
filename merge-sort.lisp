;;;
;;; Merge sort
;;;

(declaim (inline %merge))
(defun %merge (l mid r source-vec dest-vec order key)
  (declare ((integer 0 #.array-total-size-limit) l mid r)
           (function order key))
  (loop with i = l
        with j = mid
        for idx from l
        when (= i mid)
        do (loop for j from j below r
                 for idx from idx
                 do (setf (aref dest-vec idx)
                          (aref source-vec j))
                 finally (return-from %merge t))
        when (= j r)
        do (loop for i from i below mid
                 for idx from idx
                 do (setf (aref dest-vec idx)
                          (aref source-vec i))
                 finally (return-from %merge t))
        do (if (funcall order
                        (funcall key (aref source-vec i))
                        (funcall key (aref source-vec j)))
               (setf (aref dest-vec idx) (aref source-vec i)
                     i (1+ i))
               (setf (aref dest-vec idx) (aref source-vec j)
                     j (1+ j)))))

;; switch to insertion sort when sorting a short subsequence
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

;; TODO: Peephole optimization of SBCL is not sufficient to omit empty KEY
;; function. Defining deftransform will work.
(declaim (inline merge-sort!))
(defun merge-sort! (vector order &key (start 0) end (key #'identity))
  "Destructively sorts VECTOR. (You can rely on the side effect.)

ORDER := strict order"
  (declare (vector vector)
           (function order key))
  (let* ((end (or end (length vector)))
         ;; TODO: avoid to allocate a buffer of excessive size
         (buffer (make-array end :element-type (array-element-type vector))))
    (declare ((integer 0 #.array-total-size-limit) start end))
    (symbol-macrolet ((vec1 vector) (vec2 buffer))
      (labels ((recur (l r merge-to-vec1-p)
                 (declare ((mod #.array-total-size-limit) l r))
                 (cond ((= l r))
                       ((and (<= (- r l) 32) merge-to-vec1-p)
                        (%insertion-sort! vec1 order l r key))
                       ;; no-insertion-sort version
                       ;; ((= (+ l 1) r)
                       ;;  (unless merge-to-vec1-p
                       ;;    (setf (aref vec2 l) (aref vec1 l))))
                       ;; ((= (+ l 2) r)
                       ;;  (if (funcall order
                       ;;               (funcall key (aref vec1 l))
                       ;;               (funcall key (aref vec1 (- r 1))))
                       ;;      (unless merge-to-vec1-p
                       ;;        (setf (aref vec2 l) (aref vec1 l)
                       ;;              (aref vec2 (- r 1)) (aref vec1 (- r 1))))
                       ;;      (if merge-to-vec1-p
                       ;;          (rotatef (aref vec1 l) (aref vec1 (- r 1)))
                       ;;          (setf (aref vec2 l) (aref vec1 (- r 1))
                       ;;                (aref vec2 (- r 1)) (aref vec1 l)))))
                       (t (let ((mid (floor (+ l r) 2)))
                            (recur l mid (not merge-to-vec1-p))
                            (recur mid r (not merge-to-vec1-p))
                            (if merge-to-vec1-p
                                (%merge l mid r vec2 vec1 order key)
                                (%merge l mid r vec1 vec2 order key)))))))
        (recur start end t)
        vector))))
