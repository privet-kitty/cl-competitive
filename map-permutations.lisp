;;;
;;; Permutation and Combination
;;;

(declaim (inline map-permutations!))
(defun map-permutations! (function vector &optional (start 0) end)
  "Destructively permutes VECTOR[START] ... VECTOR[END-1] and applies FUNCTION
to VECTOR each time."
  (declare (vector vector))
  (labels ((recur (start end)
             (declare ((mod #.array-total-size-limit) start end))
             (if (> start end)
                 (funcall function vector)
                 (progn
                   (recur (1+ start) end)
                   (loop for i from (1+ start) below end
                         do (rotatef (aref vector start) (aref vector i))
                            (recur (1+ start) end)
                            (rotatef (aref vector start) (aref vector i)))))))
    (recur start (or end (length vector)))))

;; NOTE: It tends to be slow on SBCL version earlier than 1.5.0, as
;; constant-folding of ARRAY-ELEMENT-TYPE doesn't work.
(declaim (inline map-combinations))
(defun map-combinations (function vector length)
  "Applies FUNCTION to each combination of given length of VECTOR. Note that the
vector passed to FUNCTION will be recycled."
  (declare (vector vector)
           ((integer 0 #.most-positive-fixnum) length))
  (assert (<= length (length vector)))
  (let* ((n (length vector))
         (result (make-array length :element-type (array-element-type vector))))
    (declare ((mod #.array-total-size-limit) length))
    (labels ((recur (pos prev)
               (if (= pos length)
                   (funcall function result)
                   (loop for i from (1+ prev) below n
                         do (setf (aref result pos) (aref vector i))
                            (recur (1+ pos) i)))))
      (recur 0 -1))))

(declaim (inline map-permutations))
(defun map-permutations (function vector &optional k)
  "Applies FUNCTION to each permutation of length K of VECTOR. Note that the
vector passed to FUNCTION will be recycled."
  (declare (vector vector))
  (map-combinations (lambda (c) (map-permutations! function c)) vector k))

(defmacro do-permutations! ((var vector &optional (start 0) end) &body body)
  `(block nil (map-permutations! (lambda (,var) ,@body) ,vector ,start ,end)))

(defmacro do-combinations ((var vector length) &body body)
  `(block nil (map-combinations (lambda (,var) ,@body) ,vector ,length)))

(defmacro do-permutations ((var vector &optional k) &body body)
  `(block nil (map-permutations (lambda (,var) ,@body) ,vector ,k)))
