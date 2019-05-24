(defun delete-adjacent-duplicates (seq &key (test #'eql))
  "Destructively deletes adjacent duplicates of SEQ: e.g. #(1 1 1 2 2 1 3) ->
#(1 2 1 3)"
  (declare (function test))
  (etypecase seq
    (vector
     (if (zerop (length seq))
         seq
         (let ((prev (aref seq 0))
               (end 1))
           (loop for pos from 1 below (length seq)
                 unless (funcall test prev (aref seq pos))
                 do (setf prev (aref seq pos)
                          (aref seq end) (aref seq pos)
                          end (+ 1 end)))
           (adjust-array seq end))))
    (list (loop for rest on seq
                unless (and (cdr rest)
                            (funcall test (first rest) (second rest)))
                collect (car rest)))))

(defun map-adjacent-duplicates (function seq &key (test #'eql))
  (declare (function test function))
  (etypecase seq
    (vector
     (unless (zerop (length seq))
       (let ((prev (aref seq 0))
             (start 0))
         (loop for pos from 1 below (length seq)
               unless (funcall test prev (aref seq pos))
               do (funcall function prev (- pos start))
                  (setf prev (aref seq pos)
                        start pos)
               finally (funcall function prev (- pos start))))))
    (list (when (cdr seq)
            (labels ((recur (lst prev count)
                       (declare ((integer 0 #.most-positive-fixnum) count))
                       (cond ((null lst)
                              (funcall function prev count))
                             ((funcall test prev (car lst))
                              (recur (cdr lst) prev (+ 1 count)))
                             (t (funcall function prev count)
                                (recur (cdr lst) (car lst) 1)))))
              (recur (cdr seq) (car seq) 1))))))
