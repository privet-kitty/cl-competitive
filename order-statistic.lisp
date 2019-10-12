;;;
;;; Select i-th order statistic in O(n) (unfinished)
;;;

;; REVIEW: Is the average time complexity of SELECT-ITH! really O(n)?

(declaim (inline %hoare-partition!))
(defun %hoare-partition! (vector l r pivot-idx order)
  "Destructively rearranges VECTOR[L, R] and returns an index i: All the
elements smaller than VECTOR[PIVOT-IDX] are moved to the front of i and those
larger behind i. Note that this function deals with the **closed** interval [L,
R]."
  (declare (vector vector)
           ((integer 0 #.most-positive-fixnum) l r))
  (rotatef (aref vector pivot-idx)
           (aref vector l))
  (let ((pivot (aref vector l))
        (i l)
        (j (+ r 1)))
    (declare (fixnum i j))
    (loop
      (loop do (incf i)
            while (and (<= i r)
                       (funcall order (aref vector i) pivot)))
      (loop do (decf j)
            while (funcall order pivot (aref vector j)))
      (when (>= i j)
        (rotatef (aref vector l) (aref vector j))
        (return j))
      (rotatef (aref vector i) (aref vector j)))))

;; FIXME: When we use Lomuto partition scheme for SELECT-ITH!, it takes O(n^2)
;; time for a vector containing only the same elements like #(1 1 1 1 1).
(defun %lomuto-partition! (vector l r pivot-idx order)
  (rotatef (aref vector r) (aref vector pivot-idx))
  (let ((pivot (aref vector r))
        (base l))
    (loop for j from l below r
          unless (funcall order pivot (aref vector j))
          do (rotatef (aref vector base) (aref vector j))
             (incf base))
    (rotatef (aref vector base) (aref vector r))
    base))

(declaim (inline select-ith!))
(defun select-ith! (vector order i &optional (start 0) end)
  "Returns the (0-based) i-th smallest element of VECTOR (if order is #'<, for
  example). Destructively modifies VECTOR."
  (declare (vector vector)
           ((integer 0 #.most-positive-fixnum) i start))
  (assert (< i (length vector)))
  (labels ((recur (l r i)
             (declare ((integer 0 #.most-positive-fixnum) l r i))
             ;; (dbg l r i vector)
             (when (= l r)
               (return-from recur (aref vector r)))
             (let* ((pivot-idx (+ l (random (+ 1 (- r l)))))
                    (mid (%hoare-partition! vector l r pivot-idx order))
                    (delta (- mid l)))
               (declare ((integer 0 #.most-positive-fixnum) pivot-idx))
               ;; (dbg vector)
               (cond ((= i delta)
                      (return-from recur (aref vector mid)))
                     ((< i delta)
                      (recur l (- mid 1) i))
                     (t
                      (recur (+ mid 1) r (- i delta 1)))))))
    (recur start (or end (- (length vector) 1)) i)))

;; (defun test ()
;;   (let ((vector (make-array 50 :element-type 'fixnum)))
;;     (dotimes (i (length vector))
;;       (setf (aref vector i) (random 10)))
;;     (let ((sorted (sort (copy-seq vector) #'<)))
;;       (dotimes (i (length vector))
;;         (assert (= (aref sorted i)
;;                    (select-ith! (copy-seq vector) #'< i)))))))
