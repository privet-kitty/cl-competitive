(defun make-com-table (size &optional (divisor 1000000007))
  (declare ((unsigned-byte 31) divisor))
  (let* ((table (make-array (list size size)
                            :element-type '(unsigned-byte 31) ; FIXME
                            :initial-element 0)))
    (setf (aref table 0 0) 1)
    (loop for i from 1 below size
          do (setf (aref table i 0) 1)
             (loop for j from 1 below size
                   do (setf (aref table i j)
                            (mod (+ (aref table (- i 1) (- j 1))
                                    (aref table (- i 1) j))
                                 divisor))))
    table))

(declaim ((simple-array (unsigned-byte 31) (* *)) com-table))
(defparameter com-table (make-com-table 1000))
