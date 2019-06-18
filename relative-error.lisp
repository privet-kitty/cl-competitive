(declaim (inline relative-error-p))
(defun relative-error<= (x y threshold)
  "Returns true if the relative error between X and Y is equal to or smaller
than THRESHOLD: i.e. the relative errors of any numbers in the interval [X,
Y] (or [Y, X]) are equal to or smaller than THRESHOLD when the true value is in
the same interval."
  (and (not (zerop x))
       (not (zerop y))
       (<= (abs (/ (- x y) y)) threshold)
       (<= (abs (/ (- x y) x)) threshold)))
