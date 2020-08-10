;;;
;;; P(n, k) is the number of ways of writing n as a sum of at most k
;;; non-negative integers.
;;; 
;;; corner cases:
;;; P(0, k) = 1
;;; P(n, 0) = 0 (n != 0)
;;; P(n, k) = P(n, n) (k > n)
;;;

(declaim (inline make-partition-number-table))
(defun make-partition-number-table
    (sup-n sup-k modulus &key (element-type '(unsigned-byte 31)))
  "Builds table using the recurrence relation P(n, k) = P(n, k-1) + P(n-k,
k)."
  (declare ((mod #.array-total-size-limit) sup-n sup-k)
           ((integer 1 #.most-positive-fixnum) modulus))
  (let ((res (make-array (list sup-n sup-k) :element-type element-type)))
    (dotimes (k sup-k)
      (setf (aref res 0 k) 1))
    (loop for n from 1 below sup-n
          do (loop for k from 1 below sup-k
                   do (if (> k n)
                          (setf (aref res n k)
                                (aref res n n))
                          (setf (aref res n k)
                                (mod (+ (aref res n (- k 1))
                                        (aref res (- n k) k))
                                     modulus)))))
    res))

;; TODO: arbitrary element-type
(declaim (ftype (function * (values (simple-array (unsigned-byte 31) (*)) &optional))
                make-partition-number-sequence))
(defun make-partition-number-sequence (sup-n modulus)
  (declare ((mod #.array-total-size-limit) sup-n)
           ((unsigned-byte 31) modulus))
  (let ((res (make-array sup-n :element-type '(unsigned-byte 31) :initial-element 0)))
    (setf (aref res 0) 1)
    (loop for n from 1 below sup-n
          for value of-type fixnum = 0
          for sqrt = (isqrt (+ 1 (* 24 n)))
          do (loop for j from 1 to (floor (+ sqrt 1) 6)
                   for index = (- n (floor (* j (- (* 3 j) 1)) 2))
                   when (oddp j)
                   do (incf value (aref res index))
                   else
                   do (decf value (aref res index)))
             (loop for j from 1 to (floor (- sqrt 1) 6)
                   for index = (- n (floor (* j (+ (* 3 j) 1)) 2))
                   when (oddp j)
                   do (incf value (aref res index))
                   else
                   do (decf value (aref res index)))
             (setf (aref res n) (mod value modulus)))
    res))
