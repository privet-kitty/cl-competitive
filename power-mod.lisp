(declaim (inline power-mod))
(defun power-mod (base power &optional (divisor 1000000007))
  "BASE := integer
POWER, DIVISOR := non-negative fixnum"
  (declare ((integer 0 #.most-positive-fixnum) divisor)
           (integer base))
  (labels ((recur (x p)
             (cond ((zerop p) 1)
                   ((evenp p) (recur (mod (* x x) divisor) (ash p -1)))
                   (t (mod (* x (recur x (- p 1))) divisor)))))
    (declare (ftype (function ((unsigned-byte 32) (integer 0 #.most-positive-fixnum))
                              (values (integer 0 #.most-positive-fixnum) &optional))
                    recur))
    (recur (mod base divisor) power)))
