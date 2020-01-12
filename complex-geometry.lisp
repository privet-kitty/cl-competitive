;;;
;;; Utilities for 2D geometry (using complex number)
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


(declaim (inline calc-circumcenter))
(defun calc-circumcenter (p1 p2 p3)
  "Returns the center of circumcirlce if it exists, otherwise returns NIL."
  (declare (complex p1 p2 p3))
  (let* ((x1 (realpart p1))
         (y1 (imagpart p1))
         (x2 (realpart p2))
         (y2 (imagpart p2))
         (x3 (realpart p3))
         (y3 (imagpart p3))
         (a (+ (* x1 (- y2 y3))
               (- (* y1 (- x2 x3)))
               (* x2 y3)
               (- (* x3 y2))))
         (b (+ (* (+ (* x1 x1) (* y1 y1)) (- y3 y2))
               (* (+ (* x2 x2) (* y2 y2)) (- y1 y3))
               (* (+ (* x3 x3) (* y3 y3)) (- y2 y1))))
         (c (+ (* (+ (* x1 x1) (* y1 y1)) (- x2 x3))
               (* (+ (* x2 x2) (* y2 y2)) (- x3 x1))
               (* (+ (* x3 x3) (* y3 y3)) (- x1 x2)))))
    (handler-bind ((error (lambda (c)
                            (declare (ignore c))
                            (return-from calc-circumcenter nil))))
      (complex (- (/ b (* 2 a)))
               (- (/ c (* 2 a)))))))
