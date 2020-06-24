;;;
;;; Killer against SBCL's hash-table
;;;

(defun make-killer-list (size max)
  "Returns a killer sequence of keys. MAX should be sufficiently large. (say, >=
10^17)"
  (declare ((integer 0 #.most-positive-fixnum) size max))
  (labels ((power-of-two-floor (x) (ash 1 (- (integer-length x) 1))))
    (let* ((max (power-of-two-floor max))
           (min (power-of-two-floor (floor max size))))
      (declare ((integer 0 #.most-positive-fixnum) max min))
      (assert (zerop (mod max min)))
      (loop for i from 1 to size
            collect (* min i)))))
