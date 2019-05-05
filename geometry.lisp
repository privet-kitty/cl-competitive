;;;
;;; Routines for geometry
;;;


(declaim (inline intersect-p))
(defun intersect-p (p1-x p1-y p2-x p2-y q1-x q1-y q2-x q2-y)
  "Returns true if the line segment from (P1-X, P1-Y) to (P2-X, P2-Y) intersects
the the one from (Q1-X, Q1-Y), to (Q2-X, Q2-Y) and false otherwise."
  (let* ((delta-p-x (- p2-x p1-x))
         (delta-p-y (- p2-y p1-y))
         (delta-q-x (- q2-x q1-x))
         (delta-q-y (- q2-y q1-y))
         (det1 (* (- (* delta-p-x (- q1-y p1-y)) (* delta-p-y (- q1-x p1-x)))
                  (- (* delta-p-x (- q2-y p1-y)) (* delta-p-y (- q2-x p1-x)))))
         (det2 (* (- (* delta-q-x (- p1-y q1-y)) (* delta-q-y (- p1-x q1-x)))
                  (- (* delta-q-x (- p2-y q1-y)) (* delta-q-y (- p2-x q1-x))))))
    (and (< det1 0) (< det2 0))))
