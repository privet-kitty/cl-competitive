(declaim (inline log2-ceil))
(defun log2-ceil (x)
  "Rounds up log2(x)."
  (integer-length (- (ceiling x) 1)))

(declaim (inline log-ceil))
(defun log-ceil (x base)
  "Rounds up log(x) for BASE."
  (declare (real x)
           ((integer 1) base))
  (labels ((%log ()
             (nth-value 0 (ceiling (log x base)))))
    (if (integerp x)
        (let ((y x)
              (result 0))
          (loop (when (zerop y)
                  (return result))
                (multiple-value-bind (quot rem) (floor y base)
                  (unless (zerop rem)
                    (return (%log)))
                  (setq y quot)
                  (incf result))))
        (%log))))

