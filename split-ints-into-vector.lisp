;; obsolete
(declaim (inline split-ints-into-vector))
(defun split-ints-into-vector (string dest-vector &key (offset 0) (key #'identity))
  (declare (string string)
           (function key)
           ((array * (*)) dest-vector)
           ((integer 0 #.most-positive-fixnum) offset))
  (loop with position = 0
        for idx from offset below (length dest-vector)
        do (setf (values (aref dest-vector idx) position)
                 (parse-integer string :start position :junk-allowed t))
           (setf (aref dest-vector idx) (funcall key (aref dest-vector idx)))
        finally (return dest-vector)))
