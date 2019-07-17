;;;
;;; Geometry functions for complex number
;;;

(defun calc-internal-angle (c1 c2)
  (acos (max -1d0 (min 1d0 (/ (+ (* (realpart c1) (realpart c2))
                                 (* (imagpart c1) (imagpart c2)))
                              (* (abs c1) (abs c2)))))))

;; The range of PHASE is (-PI, PI]
(declaim (inline calc-angle))
(defun calc-angle (c1 c2)
  "Returns the anticlockwise angle from vector C1 to vector C2. The range is [0,
2PI)."
  (mod (- (phase c2) (phase c1)) #.(* 2 PI)))
