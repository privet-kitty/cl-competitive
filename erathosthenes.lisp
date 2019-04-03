(declaim (ftype (function * (values simple-bit-vector &optional)) make-prime-table))
(defun make-prime-table (size)
  "Erzeugt die Primzahlentabelle 0 zu SIZE-1."
  (declare (optimize (speed 3)))
  (let ((dict (make-array size :element-type 'bit :initial-element 1)))
    (setf (sbit dict 0) 0
          (sbit dict 1) 0)
    (loop for even-num from 4 below size by 2
          do (setf (sbit dict even-num) 0))
    (loop for p from 3 to (ceiling (sqrt size)) by 2
          when (= 1 (sbit dict p))
          do (loop for composite from (+ p p) below size by p
                   until (>= composite size)
                   do (setf (sbit dict composite) 0)))
    dict))

(declaim (inline decompose-to-pow-table))
(defun decompose-to-pow-table (num prime-table)
  (declare (optimize (speed 3))
           ((integer 1) num)
           (simple-bit-vector prime-table))
  (let ((factor-table (make-array (length prime-table)
                                  :element-type '(unsigned-byte 32)
                                  :initial-element 0)))
    (when (> (length prime-table) 2)
      (setf (aref factor-table 2)
            (loop while (evenp num)
                  count t
                  do (setf num (ash num -1)))))
    (loop for prime from 3 to (min num (- (length prime-table) 1)) by 2
          when (= 1 (sbit prime-table prime))
          do (setf (aref factor-table prime)
                   (loop with quot and rem
                         do (setf (values quot rem) (floor num prime))
                         while (zerop rem)
                         count t
                         do (setf num quot)))
          finally (return factor-table))))

