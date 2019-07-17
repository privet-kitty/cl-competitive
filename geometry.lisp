;;;
;;; Utilities for geometry
;;;

;; TODO: introduce eps
(declaim (inline intersect-p))
(defun intersect-p (p1-x p1-y p2-x p2-y q1-x q1-y q2-x q2-y)
  "Returns true if the line segment from (P1-X, P1-Y) to (P2-X, P2-Y) intersects
the the one from (Q1-X, Q1-Y) to (Q2-X, Q2-Y) and false otherwise."
  (let* ((delta-p-x (- p2-x p1-x))
         (delta-p-y (- p2-y p1-y))
         (delta-q-x (- q2-x q1-x))
         (delta-q-y (- q2-y q1-y))
         (det1 (* (- (* delta-p-x (- q1-y p1-y)) (* delta-p-y (- q1-x p1-x)))
                  (- (* delta-p-x (- q2-y p1-y)) (* delta-p-y (- q2-x p1-x)))))
         (det2 (* (- (* delta-q-x (- p1-y q1-y)) (* delta-q-y (- p1-x q1-x)))
                  (- (* delta-q-x (- p2-y q1-y)) (* delta-q-y (- p2-x q1-x))))))
    (and (< det1 0) (< det2 0))))

(declaim (inline calc-internal-angle))
(defun calc-internal-angle (x1 y1 x2 y2)
  (let ((x (/ (+ (* x1 x2) (* y1 y2))
              (* (sqrt (+ (* x1 x1) (* y1 y1)))
                 (sqrt (+ (* x2 x2) (* y2 y2)))))))
    ;; avoid complex angle
    (acos (max -1d0 (min 1d0 x)))))

(declaim (inline calc-angle))
(defun calc-angle (x1 y1 x2 y2)
  "Returns the anticlockwise angle from p1 to p2. The range is [0, 2PI)."
  (mod (- (atan y2 x2) (atan y1 x1))
       #.(* 2 pi)))
