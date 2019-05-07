(declaim (ftype (function * (values (vector (integer 0 #.most-positive-fixnum)) &optional))
                enum-divisors))
(defun enum-divisors (x)
  "Enumerates all the divisors of X. Note that the returned vector is NOT
sorted."
  (declare ((integer 0 #.most-positive-fixnum) x))
  (let* ((sqrt (isqrt x))
         (res (make-array (isqrt sqrt) ; FIXME: sets the initial size to x^1/4
                          :element-type '(integer 0 #.most-positive-fixnum)
                          :fill-pointer 0)))
    (loop for i from 1 to sqrt
          do (multiple-value-bind (quot rem) (floor x i)
               (when (zerop rem)
                 (vector-push-extend i res)
                 (unless (= i quot)
                   (vector-push-extend quot res)))))
    res))
