;; From alexandria
(declaim (inline clamp))
(defun clamp (number min max)
  (if (< number min)
      min
      (if (> number max)
          max
          number)))
