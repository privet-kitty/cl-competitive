(declaim (inline power-mod))
(defun power-mod (base power &optional (divisor 1000000007))
  "BASE := integer
POWER, DIVISOR := non-negative fixnum"
  (declare ((integer 0 #.most-positive-fixnum) divisor power)
           (integer base))
  (labels ((recur (x p)
             (declare ((integer 0 #.most-positive-fixnum) x p)
                      (values (integer 0 #.most-positive-fixnum)))
             (cond ((zerop p) 1)
                   ((evenp p) (recur (mod (* x x) divisor) (ash p -1)))
                   (t (mod (* x (recur x (- p 1))) divisor)))))
    (recur (mod base divisor) power)))
