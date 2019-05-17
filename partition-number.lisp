;;;
;;; P(n, k) is the number of ways of writing n as a sum of at least k
;;; non-negative integers.
;;; 
;;; corner cases:
;;; P(0, k) = 1
;;; P(n, 0) = 0 (n != 0)
;;; P(n, k) = P(n, n) (k > n)
;;;

(defconstant +partition-size+ 1100)
(defconstant +partition-mod+ #.(+ (expt 10 9) 7))

(declaim ((simple-array (unsigned-byte 32) (#.+partition-size+ #.+partition-size+)) *partition*))
(defparameter *partition*
  (make-array (list +partition-size+ +partition-size+)
              :element-type '(unsigned-byte 32)
              :initial-element 0))

(defun initialize-partition ()
  "Fills *PARTITION* using P(n, k) = P(n, k-1) + P(n-k, k)."
  (dotimes (k +partition-size+)
    (setf (aref *partition* 0 k) 1))
  (loop for n from 1 below +partition-size+
        do (setf (aref *partition* n 0) 0)
           (loop for k from 1 to n
                 do (setf (aref *partition* n k)
                          (mod (+ (aref *partition* n (- k 1))
                                  (aref *partition* (- n k) k))
                               +partition-mod+)))
           (loop for k from (+ n 1) below +partition-size+
                 do (setf (aref *partition* n k)
                          (aref *partition* n n)))))

(initialize-partition)

