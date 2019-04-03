(defun modfactorial (n divisor)
  (declare (optimize (speed 3)) ; TCO
           ((integer 0) n divisor))
  (labels ((rec (n res)
             (declare ((integer 0) n res))
             (if (zerop n)
                 res
                 (rec (- n 1) (mod (* res n) divisor)))))
    (rec n 1)))
