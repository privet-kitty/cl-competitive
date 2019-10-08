;;;
;;; S(n, k): Stirling number of the second kind
;;;

(defun make-stirling2-table (size1 size2 modulus)
  (declare (optimize (speed 3))
           ((unsigned-byte 32) size1 size2 modulus))
  (let ((table (make-array (list size1 size2)
                           :element-type '(unsigned-byte 32))))
    (setf (aref table 0 0) 1)
    (loop for n from 1 below size1
          do (loop for k from 1 below (min (+ n 1) size2)
                   do (setf (aref table n k)
                            (mod (+ (aref table (- n 1) (- k 1))
                                    (mod (* k (aref table (- n 1) k))
                                         modulus))
                                 modulus))))
    table))
