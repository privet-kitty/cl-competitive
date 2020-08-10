;;;
;;; Freiwalds' algorithm
;;;

;; NOTE: not tested
(declaim (inline gemm-p))
(defun gemm-p (a b c &optional (count 32))
  "Always returns true if AB = C. If AB != C, returns false with probability at
least 1 - (1/2)^COUNT."
  (declare ((integer 0 #.most-positive-fixnum) count)
           ((array * (* *)) a b c))
  (let* ((dim1 (array-dimension a 0))
         (dim2 (array-dimension a 1))
         (dim3 (array-dimension b 1))
         (rand-vector (make-array dim3 :element-type 'bit :initial-element 0))
         (tmp-vector (make-array dim2 :element-type (array-element-type b))))
    (dotimes (_ count)
      (fill tmp-vector 0)
      (dotimes (k dim3)
        (setf (aref rand-vector k) (random 2)))
      (dotimes (j dim2)
        (let ((sum 0))
          (dotimes (k dim3)
            (when (= 1 (sbit rand-vector k))
              (incf sum (aref b j k))))
          (setf (aref tmp-vector j) sum)))
      (dotimes (i dim1)
        (let ((left-sum 0)
              (right-sum 0))
          (dotimes (j dim2)
            (incf left-sum (* (aref a i j) (aref tmp-vector j))))
          (dotimes (k dim3)
            (when (= 1 (sbit rand-vector k))
              (incf right-sum (aref c i k))))
          (unless (= left-sum right-sum)
            (return-from gemm-p nil)))))
    t))
