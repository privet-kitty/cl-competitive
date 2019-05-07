(declaim (inline count-digits))
(defun count-digits (x)
  "Counts the number of digits of the decimal X."
  (declare ((integer 0 #.most-positive-fixnum) x))
  (do ((n 0 (1+ n))
       (x x (floor x 10)))
      ((zerop x) n)
    (declare ((integer 0 #.most-positive-fixnum) n x))))
