(declaim (inline xor-sum))
(defun xor-sum (x)
  "Returns the xor of 1, 2, ..., X-1"
  (declare (unsigned-byte x))
  (case (logand 3 x)
    (0 0)
    (1 (- x 1))
    (2 1)
    (otherwise x)))
