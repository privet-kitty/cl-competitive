;; obsolete
(declaim (inline split-ints-into-array))
(defun split-ints-into-array (string dest-array row &key (offset 0) (key #'identity))
  (declare (string string)
           (function key)
           ((array * (* *)) dest-array)
           ((integer 0 #.most-positive-fixnum) row offset))
  (loop with position = 0
        for idx from offset below (array-dimension dest-array 1)
        do (setf (values (aref dest-array row idx) position)
                 (parse-integer string :start position :junk-allowed t))
           (setf (aref dest-array row idx)
                 (funcall key (aref dest-array row idx)))
        finally (return dest-array)))
