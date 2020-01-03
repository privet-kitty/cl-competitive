;; not tested

;; TODO: integrate these two functions
(declaim (inline map-monotone-subseq))
(defun map-monotone-subseq (function vector order)
  "Applies FUNCTION to each monotone subsequence (or substring, strictly
speaking) of VECTOR. FUNCTION must take two arguments, L and R, where the
interval [L, R) corresponds to a monotone subsequence.

SEQ[l, r) is monotone iff (FUNCALL ORDER SEQ[i] SEQ[i+1]) always returns
true in this range."
  (declare (vector vector)
           (function function order))
  (unless (zerop (length vector))
       (let ((prev 0))
         (loop for i from 1 below (length vector)
               unless (funcall order (aref vector (- i 1)) (aref vector i))
               do (funcall function prev i)
                  (setq prev i)
               finally (funcall function prev (length vector))))))

(declaim (inline map-altering-monotone-subseq))
(defun map-altering-monotone-subseq (function vector order)
  "Alternately applies FUNCTION to each increasing subsequence and decreasing
one (if ORDER is #'<, for example). FUNCTION must take two arguments, L and R,
where the interval [L, R) corresponds to a monotone subsequence.

`Decreasing and then increasing' can also be realized by passing a descending
order (e.g. #'>) to ORDER. Non-decreasing (#'<=) and non-increasing (#'>=) are
also allowed."
  (declare (vector vector))
  (let ((prev 0)
        (up t))
    (declare ((integer 0 #.most-positive-fixnum) prev))
    (unless (zerop (length vector))
      (loop for i from 1 below (length vector)
            do (if up
                   (unless (funcall order (aref vector (- i 1)) (aref vector i))
                     (funcall function prev i)
                     (setq prev i
                           up nil))
                   (unless (funcall order (aref vector i) (aref vector (- i 1)))
                     (funcall function prev i)
                     (setq prev i
                           up t)))
            finally (funcall function prev (length vector))))))
