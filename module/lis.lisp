(defpackage :cp/lis
  (:use :cl)
  (:export #:calc-lis))
(in-package :cp/lis)

;;;
;;; Longest increasing/decreasing/non-decreasing/non-increasing subsequence
;;;

;; TODO: smarter restoring
(declaim (inline calc-lis))
(defun calc-lis (vector order infinity &optional restore)
  "Computes the length of LIS in O(Nlog(N)) time. Returns a LIS as the second
value when RESTORE is true.

Order:
<: increasing
>: decreasing
<=: non-decreasing
>=: non-increasing

Example:
> (calc-lis #(1 0 -5 0 -5 4 4 -4 -1 -2) #'< most-positive-fixnum t)
=> 3
#(-5 -4 -2)"
  (declare (vector vector))
  (let* ((n (length vector))
         (dp (make-array (+ 1 n)
                         :element-type (array-element-type vector)
                         :initial-element infinity))
         (end 0))
    (declare ((integer 0 #.array-total-size-limit) end))
    (labels ((bisect (ng ok value)
               (declare ((integer -1 #.array-total-size-limit) ng ok))
               (if (<= (- ok ng) 1)
                   ok
                   (let ((mid (ash (+ ng ok) -1)))
                     (if (funcall order (aref dp mid) value)
                         (bisect mid ok value)
                         (bisect ng mid value))))))
      (if restore
          (let ((prevs (make-array n :element-type 'fixnum :initial-element -1))
                (indices (make-array n :element-type '(integer 0 #.most-positive-fixnum))))
            (loop for i below n
                  for x = (aref vector i)
                  for pos = (bisect -1 end x)
                  do (setf (aref dp pos) x
                           (aref indices pos) i)
                  unless (zerop pos)
                  do (setf (aref prevs i) (aref indices (- pos 1)))
                  when (= end pos)
                  do (incf end))
            (let ((res (make-array end :element-type (array-element-type vector))))
              (when (zerop end)
                (return-from calc-lis (values end res)))
              (loop with index = (aref indices (- end 1))
                    for i from (- end 1) downto 0
                    do (setf (aref res i) (aref vector index)
                             index (aref prevs index))
                    finally (return (values end res)))))
          (loop for x across vector
                for pos = (bisect -1 end x)
                do (setf (aref dp pos) x)
                when (= end pos)
                do (incf end)
                finally (return (values end nil)))))))
