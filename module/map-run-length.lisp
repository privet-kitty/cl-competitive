(declaim (inline map-run-length))
(defun map-run-length (function seq &key (test #'eql))
  "Applies FUNCTION to each equal successive element of SEQ. FUNCTION must take
two arguments: the first one receives an element in SEQ and the second one
receives the number of the successive elements equal to the first.

Example: (map-run-length (lambda (x c) (format t \"~D ~D~%\" x c)) #(1 1 1 2 2 1 3))
1 3
2 2
1 1
3 1
"
     (declare (sequence seq)
              (function test function))
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
       (list
        (when (cdr seq)
          (labels ((recur (lst prev count)
                     (declare ((integer 0 #.most-positive-fixnum) count))
                     (cond ((null lst)
                            (funcall function prev count))
                           ((funcall test prev (car lst))
                            (recur (cdr lst) prev (+ 1 count)))
                           (t (funcall function prev count)
                              (recur (cdr lst) (car lst) 1)))))
            (recur (cdr seq) (car seq) 1))))))
