;;;
;;; GCD and LCM
;;; Reference:
;;; https://lemire.me/blog/2013/12/26/fastest-way-to-compute-the-greatest-common-divisor/
;;;

(declaim (inline %fast-gcd fast-gcd fast-lcm)
         (ftype (function * (values (integer 0 #.most-positive-fixnum) &optional))
                %fast-gcd fast-gcd fast-lcm))
(defun %fast-gcd (u v)
  (declare ((integer 0 #.most-positive-fixnum) u v))
  (let ((shift (let ((x (logior u v)))
                 (- (integer-length (logand x (- x))) 1))))
    (setq u (ash u (- 1 (integer-length (logand u (- u))))))
    (loop (setq v (ash v (- 1 (integer-length (logand v (- v))))))
          (when (> u v)
            (rotatef u v))
          (decf v u)
          (when (zerop v)
            (return (the (integer 1 #.most-positive-fixnum)
                         (ash u shift)))))))

(defun fast-gcd (u v)
  (declare ((integer 0 #.most-positive-fixnum) u v))
  (cond ((zerop u) v)
        ((zerop v) u)
        (t (%fast-gcd u v))))

(defun fast-lcm (u v)
  (declare ((integer 0 #.most-positive-fixnum) u v))
  (if (or (zerop u) (zerop v))
      0
      (multiple-value-bind (max min)
          (if (> u v)
              (values u v)
              (values v u))
        (* (truncate max (%fast-gcd u v)) min))))
