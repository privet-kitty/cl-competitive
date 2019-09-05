;;;
;;; Compute inversion number by merge sort
;;;

(declaim (inline %merge-count))
(defun %merge-count (l mid r source-vec dest-vec predicate)
  (declare ((integer 0 #.array-total-size-limit) l mid r)
           (function predicate))
  (loop with count of-type (integer 0 #.most-positive-fixnum) = 0
        with i = l
        with j = mid
        for idx from l
        when (= i mid)
        do (loop for j from j below r
                 for idx from idx
                 do (setf (aref dest-vec idx)
                          (aref source-vec j))
                 finally (return-from %merge-count count))
        when (= j r)
        do (loop for i from i below mid
                 for idx from idx
                 do (setf (aref dest-vec idx)
                          (aref source-vec i))
                 finally (return-from %merge-count count))
        do (if (funcall predicate
                        (aref source-vec j)
                        (aref source-vec i))
               (setf (aref dest-vec idx) (aref source-vec j)
                     j (1+ j)
                     count (+ count (- mid i)))
               (setf (aref dest-vec idx) (aref source-vec i)
                     i (1+ i)))))

(defmacro with-fixnum+ (form)
  (let ((fixnum+ '(integer 0 #.most-positive-fixnum)))
    `(the ,fixnum+
          ,(reduce (lambda (f1 f2)`(,(car form)
                                   (the ,fixnum+ ,f1)
                                   (the ,fixnum+ ,f2)))
	           (cdr form)))))

(declaim (inline %calc-by-insertion-sort!))
(defun %calc-by-insertion-sort! (vec predicate l r)
  (declare (function predicate)
           ((integer 0 #.array-total-size-limit) l r))
  (loop with inv-count of-type (integer 0 #.most-positive-fixnum) = 0
        for end from (+ l 1) below r
        do (loop for i from end above l
                 while (funcall predicate (aref vec i) (aref vec (- i 1)))
                 do (rotatef (aref vec (- i 1)) (aref vec i))
                    (incf inv-count))
        finally (return inv-count)))

;; NOTE: This function is slow on SBCL version earlier than 1.5.0 as
;; constant-folding of ARRAY-ELEMENT-TYPE doesn't work. Use
;; array-element-type.lisp if necessary.
(declaim (inline count-inversions!))
(defun count-inversions! (vector predicate &key (start 0) end)
  "Calculates the number of the inversions of VECTOR w.r.t. the strict order
PREDICATE. This function sorts VECTOR as a side effect."
  (declare (vector vector)
           (function predicate))
  (let ((end (or end (length vector))))
    (declare ((integer 0 #.array-total-size-limit) start end))
    (assert (<= start end))
    (let ((buffer (make-array end :element-type (array-element-type vector))))
      (labels
          ((recur (l r merge-to-vec1-p)
             (declare (optimize (safety 0))
                      ((integer 0 #.array-total-size-limit) l r))
             (cond ((= l r) 0)
                   ((= (+ l 1) r)
                    (unless merge-to-vec1-p
                      (setf (aref buffer l) (aref vector l)))
                    0)
                   ;; It is faster to use insertion sort. I don't adopt it
                   ;; by default, however, because that makes it hard to
                   ;; change the code to fit some special settings.
                   ;; ((and (<= (- r l) 24) merge-to-vec1-p)
                   ;;  (%calc-by-insertion-sort! vector predicate l r))
                   (t
                    (let ((mid (floor (+ l r) 2)))
                      (with-fixnum+
                          (+ (recur l mid (not merge-to-vec1-p))
                             (recur mid r (not merge-to-vec1-p))
                             (if merge-to-vec1-p
                                 (%merge-count l mid r buffer vector predicate)
                                 (%merge-count l mid r vector buffer predicate)))))))))
        (recur start end t)))))
