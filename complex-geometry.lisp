;;;
;;; Utilities for 2D geometry (using complex number)
;;;

(declaim (inline intersect-p))
(defun intersect-p (p1 p2 q1 q2 &optional (eps 0))
  "Returns true iff the line segment from P1 to P2 intersects the one from Q1
to Q2."
  (let* ((p1-x (realpart p1))
         (p1-y (imagpart p1))
         (p2-x (realpart p2))
         (p2-y (imagpart p2))
         (q1-x (realpart q1))
         (q1-y (imagpart q1))
         (q2-x (realpart q2))
         (q2-y (imagpart q2))
         (delta-p-x (- p2-x p1-x))
         (delta-p-y (- p2-y p1-y))
         (delta-q-x (- q2-x q1-x))
         (delta-q-y (- q2-y q1-y))
         (det1 (* (- (* delta-p-x (- q1-y p1-y)) (* delta-p-y (- q1-x p1-x)))
                  (- (* delta-p-x (- q2-y p1-y)) (* delta-p-y (- q2-x p1-x)))))
         (det2 (* (- (* delta-q-x (- p1-y q1-y)) (* delta-q-y (- p1-x q1-x)))
                  (- (* delta-q-x (- p2-y q1-y)) (* delta-q-y (- p2-x q1-x))))))
    (and (<= det1 eps) (<= det2 eps))))

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

(declaim (inline cross-product))
(defun cross-product (p1 p2)
  (- (* (realpart p1) (imagpart p2))
     (* (imagpart p1) (realpart p2))))

(declaim (inline on-line-segment-p))
(defun on-line-segment-p (point end1 end2 &optional (eps 0))
  "Returns T iff POINT is on the line segment between END1 and END2."
  (and (<= (abs (cross-product (- point end1) (- end2 end1))) eps)
       (or (<= (realpart end1) (realpart point) (realpart end2))
           (<= (realpart end2) (realpart point) (realpart end1)))
       (or (<= (imagpart end1) (imagpart point) (imagpart end2))
           (<= (imagpart end2) (imagpart point) (imagpart end1)))))

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

;; Reference: https://stackoverflow.com/questions/1119627/how-to-test-if-a-point-is-inside-of-a-convex-polygon-in-2d-integer-coordinates
;; TODO: introduce eps
;; TODO: strictly/weekly inside
(declaim (inline inside-convex-polygon-p))
(defun inside-convex-polygon-p (point vector)
  "Returns T iff POINT is strictly inside the given convex polygon. Vertices of
the polygon must be ordered clockwise or anticlockwise in VECTOR."
  (declare (number point)
           (vector vector))
  (let (prev-side
        (n (length vector)))
    (labels ((get-side (a b)
               (let ((x (- (* (realpart a) (imagpart b))
                           (* (imagpart a) (realpart b)))))
                 (cond ((< x 0) -1)
                       ((> x 0) 1)
                       (t 0)))))
      (dotimes (i n)
        (let* ((a (aref vector i))
               (b (aref vector (if (= i (- n 1)) 0 (+ i 1))))
               (a-to-b (- b a))
               (a-to-point (- point a))
               (current-side (get-side a-to-b a-to-point)))
          (cond ((zerop current-side)
                 (return-from inside-convex-polygon-p nil))
                ((null prev-side)
                 (setq prev-side current-side))
                ((/= prev-side current-side)
                 (return-from inside-convex-polygon-p nil)))))
      t)))

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
