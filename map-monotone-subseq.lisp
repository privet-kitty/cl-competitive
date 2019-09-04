;; unfinished

(declaim (inline map-monotone-subseq))
(defun map-monotone-subseq (function seq order)
  "Applies FUNCTION to each monotone subsequence (or substring, strictly
speaking) of SEQ. FUNCTION must take two arguments, L and R, where the interval
[L, R) corresponds to a monotone subsequence.

SEQ[l, r) is monotone iff (FUNCALL ORDER SEQ[i] SEQ[i+1]) always returns
true in this range."
  (declare (sequence seq)
           (function function order))
  (etypecase seq
    (vector
     (unless (zerop (length seq))
       (let ((base 0))
         (loop for i from 1 below (length seq)
               unless (funcall order (aref seq (- i 1)) (aref seq i))
               do (funcall function base i)
                  (setq base i)
               finally (funcall function base (length seq))))))
    (list
     "Not implemented yet.")))
