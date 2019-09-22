(declaim (inline map-indices))
(defun map-indices (function max-indices)
  "Applies function to each vector of indices smaller than MAX-INDICES. The
consequence is undefined when the vector is modified in FUNCTION."
  (declare (vector max-indices))
  (let* ((size (length max-indices))
         (indices (make-array size :element-type (array-element-type max-indices))))
    (labels ((recur (pos)
               (declare ((integer 0 #.array-dimension-limit) pos))
               (if (= pos size)
                   (funcall function indices)
                   (dotimes (idx (the (integer 0 #.most-positive-fixnum)
                                      (aref max-indices pos)))
                     (setf (aref indices pos) idx)
                     (recur (+ pos 1))))))
      (recur 0))))

(defmacro do-indices ((var end-indices) &body body)
  "DO-style macro for MAP-INDICES"
  `(block nil (map-indices (lambda (,var) ,@body) ,end-indices)))
