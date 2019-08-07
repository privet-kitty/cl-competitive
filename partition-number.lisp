;;;
;;; P(n, k) is the number of ways of writing n as a sum of at most k
;;; non-negative integers.
;;; 
;;; corner cases:
;;; P(0, k) = 1
;;; P(n, 0) = 0 (n != 0)
;;; P(n, k) = P(n, n) (k > n)
;;;

;: TODO: non-global handling
(defconstant +partition-sum-sup+ 1100) ; exclusive upper bound of n
(defconstant +partition-sup+ 1100) ; exclusive upper bound of k
(defconstant +partition-mod+ #.(+ (expt 10 9) 7))

(declaim ((simple-array (unsigned-byte 32) (#.+partition-sum-sup+ #.+partition-sup+)) *partition*))
(defparameter *partition*
  (make-array (list +partition-sum-sup+ +partition-sup+)
              :element-type '(unsigned-byte 32)
              :initial-element 0))

(defun initialize-partition ()
  "Fills *PARTITION* using the recurrence relation P(n, k) = P(n, k-1) + P(n-k,
k)."
  (dotimes (k +partition-sup+)
    (setf (aref *partition* 0 k) 1))
  (loop for n from 1 below +partition-sum-sup+
        do (loop for k from 1 below +partition-sup+
                 do (if (> k n)
                        (setf (aref *partition* n k)
                              (aref *partition* n n))
                        (setf (aref *partition* n k)
                              (mod (+ (aref *partition* n (- k 1))
                                      (aref *partition* (- n k) k))
                                   +partition-mod+))))))

(initialize-partition)

