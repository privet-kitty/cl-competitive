;;;
;;; Z-algorithm
;;;

(declaim (inline make-z-array))
(defun make-z-array (vector &key (test #'eql))
  "Returns a vector Z of the same length as VECTOR. An element Z[i] is the
length of the longest substring starting from i which coincides with a prefix of
VECTOR."
  (declare (vector vector))
  (let* ((size (length vector))
         (base 0)
         (res (make-array size
                          :element-type '(integer 0 #.most-positive-fixnum)
                          :initial-element 0)))
    (declare ((integer 0 #.most-positive-fixnum) base))
    (loop for i from 1 below size
          do (if (< (+ i (aref res (- i base)))
                    (+ base (aref res base)))
                 (setf (aref res i)
                       (aref res (- i base)))
                 (let ((delta (max 0 (- (+ base (aref res base)) i))))
                   (declare ((integer 0 #.most-positive-fixnum) delta))
                   (loop while (and (< (+ i delta) size)
                                    (funcall test
                                             (aref vector delta)
                                             (aref vector (+ i delta))))
                         do (incf delta))
                   (setf (aref res i) delta
                         base i))))
    (unless (zerop size)
      (setf (aref res 0) size))
    res))

