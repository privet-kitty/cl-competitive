;; -*- encoding: utf-8 -*-
(declaim (inline multiply-mat))
(defun multiply-mat (mat1 mat2)
  (declare (optimize (speed 3) (safety 1)))
  (destructuring-bind (l m1) (array-dimensions mat1)
    (destructuring-bind (m2 n) (array-dimensions mat2)
      (when (/= m1 m2)
        (error "Die Dimensionen stimmen nicht überein."))
      (let ((mat (make-array (list l n) :element-type (array-element-type mat1))))
        (declare (dynamic-extent mat))
        (dotimes (row l)
          (dotimes (col n)
            (setf (aref mat row col)
                  (loop for mid from 0 below m1
                        sum (* (aref mat1 row mid)
                               (aref mat2 mid col))))))
	mat))))
	
