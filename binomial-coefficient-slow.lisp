;;
;; (Better to use binomial-coefficient-mod.) This code generates the table of
;; binomial coefficients with time and space complexity O(n^2).
;;

(defun make-binom-table (size &optional (modulus 1000000007))
  (check-type modulus (unsigned-byte 32))
  (let* ((table (make-array (list size size)
                            :element-type '(unsigned-byte 32)
                            :initial-element 0)))
    (setf (aref table 0 0) 1)
    (loop for i from 1 below size
          do (setf (aref table i 0) 1)
             (loop for j from 1 below size
                   do (setf (aref table i j)
                            (mod (+ (aref table (- i 1) (- j 1))
                                    (aref table (- i 1) j))
                                 modulus))))
    table))

(declaim ((simple-array (unsigned-byte 32) (* *)) *binom*))
(defparameter *binom* (make-binom-table 500))
