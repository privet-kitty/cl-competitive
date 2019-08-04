(declaim (inline number-length))
(defun number-length (x &optional (radix 10))
  "Returns the length of the integer X when displayed in RADIX. (Returns 0 when
X = 0.)"
  (declare (unsigned-byte x)
           ((integer 2 #.most-positive-fixnum) radix))
  (loop for length of-type (integer 0 #.most-positive-fixnum) from 0
        for y = x then (floor y radix)
        until (zerop y)
        finally (return length)))
