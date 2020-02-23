;;;
;;; Utilities for 2D geometry (using complex number)
;;;

(defun calc-internal-angle (c1 c2)
  (acos (max -1d0 (min 1d0 (/ (+ (* (realpart c1) (realpart c2))
                                 (* (imagpart c1) (imagpart c2)))
                              (* (abs c1) (abs c2)))))))

;; The range of CL:PHASE is (-PI, PI]
(declaim (inline calc-angle))
(defun calc-angle (c1 c2)
  "Returns the anticlockwise angle (in the sense of amplitude) from vector C1 to
vector C2. The range is [0, 2PI)."
  (mod (- (phase c2) (phase c1)) #.(* 2 PI)))

;; http://www.ambrsoft.com/trigocalc/circle3d.htm
(declaim (inline calc-circumcenter))
(defun calc-circumcenter (p1 p2 p3 &optional (eps 1d-8))
  "Returns the center of circumcirlce if it exists, otherwise returns NIL. May
throw a floating exception if EPS is too small."
  (declare ((real 0) eps))
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
    (if (< a eps)
        nil
        (complex (- (/ b (* 2 a)))
                 (- (/ c (* 2 a)))))))

;;;
;;; Welzl's algorithm for smallest circle problem (not optimized)
;;;
;;; Reference:
;;; Mark de Berg et al., Computational Geometry: Algorithms and Applications, 3rd Edition
;;;

(declaim (inline %mini-disc-with-2-points))
(defun %mini-disc-with-2-points (points end q1 q2 eps)
  "Returns the smallest circle that contains points, q1 and q2 (contains q1 and
q2 on the circumference)."
  (declare (vector points)
           ((integer 0 #.most-positive-fixnum) end))
  (let* ((center (* 1/2 (+ q1 q2)))
         (radius (abs (coerce (- q1 center) '(complex double-float)))))
    (dotimes (i end)
      (let ((new-point (aref points i)))
        (when (>= (abs (- new-point center)) (+ radius eps))
          (let ((new-center (calc-circumcenter q1 q2 new-point)))
            (setq center new-center
                  radius (abs (- new-point new-center)))))))
    (values center radius)))

(declaim (inline %shuffle!))
(defun %shuffle! (vector &optional end)
  "Destructively shuffles VECTOR by Fisher-Yates algorithm."
  (declare ((or null (integer 0 #.most-positive-fixnum)) end))
  (loop for i from (- (or end (length vector)) 1) above 0
        for j = (random (+ i 1))
        do (rotatef (aref vector i) (aref vector j)))
  vector)

(declaim (inline %mini-disc-with-point))
(defun %mini-disc-with-point (points end q eps)
  (declare (vector points)
           ((integer 0 #.most-positive-fixnum) end))
  (%shuffle! points end)
  (let* ((center (* 1/2 (+ (aref points 0) q)))
         (radius (abs (coerce (- q center) '(complex double-float)))))
    (loop for i from 1 below end
          for new-point of-type complex = (aref points i)
          when (>= (abs (- new-point center)) (+ radius eps))
          do (setf (values center radius)
                   (%mini-disc-with-2-points points i (aref points i) q eps)))
    (values center radius)))

(declaim (inline calc-smallest-circle))
(defun calc-smallest-circle (points eps)
  (declare (vector points))
  (assert (>= (length points) 1))
  (when (= 1 (length points))
    (return-from calc-smallest-circle
      (values (aref points 0)
              (coerce 0 (type-of (realpart (aref points 0)))))))
  (let* ((points (copy-seq (%shuffle! points)))
         (copy (copy-seq points))
         (p0 (aref points 0))
         (p1 (aref points 1))
         (center (* 1/2 (+ p0 p1)))
         (radius (abs (coerce (- p0 center) '(complex double-float)))))
    (loop for i from 2 below (length points)
          for new-point = (aref points i)
          when (>= (abs (- new-point center)) (+ radius eps))
          do (setf (values center radius)
                   (%mini-disc-with-point copy i new-point eps)))
    (values center radius)))
