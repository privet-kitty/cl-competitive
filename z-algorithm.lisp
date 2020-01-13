;;;
;;; Z-algorithm
;;;

(declaim (inline make-z-array))
(defun make-z-array (vector &key (test #'eql))
  "Returns a vector Z of the same length as VECTOR. Z[i] equals to the length of
the longest substring starting from i which coincides with a prefix of VECTOR."
  (declare (vector vector))
  (let* ((size (length vector))
         (base 0)
         (z (make-array size
                        :element-type '(integer 0 #.most-positive-fixnum)
                        :initial-element 0)))
    (declare ((integer 0 #.most-positive-fixnum) base))
    (loop for i from 1 below size
          do (if (< (+ i (aref z (- i base)))
                    (+ base (aref z base)))
                 (setf (aref z i)
                       (aref z (- i base)))
                 (let ((delta (max 0 (- (+ base (aref z base)) i))))
                   (declare ((integer 0 #.most-positive-fixnum) delta))
                   (loop while (and (< (+ i delta) size)
                                    (funcall test
                                             (aref vector delta)
                                             (aref vector (+ i delta))))
                         do (incf delta))
                   (setf (aref z i) delta
                         base i))))
    (unless (zerop size)
      (setf (aref z 0) size))
    z))

