;;
;; Generate the table of binomial coefficients with mod with time and space
;; complexity O(n^2).
;;

(defun make-binom-table (size &optional (divisor 1000000007))
  (check-type divisor (unsigned-byte 32))
  (let* ((table (make-array (list size size)
                            :element-type '(unsigned-byte 32) ; FIXME
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

(declaim ((simple-array (unsigned-byte 32) (* *)) *binom*))
(defparameter *binom* (make-binom-table 500))
