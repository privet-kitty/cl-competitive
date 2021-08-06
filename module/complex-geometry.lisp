(defpackage :cp/complex-geometry
  (:use :cl)
  (:export #:intersect-p #:calc-internal-angle #:calc-angle #:on-line-segment-p
           #:parallel-p #:overlap-p #:cross* #:dot* #:inside-convex-polygon-p #:in-circle
           #:barycentric #:convex-quad-p
           #:point< #:merge-segments #:calc-intersection #:canonize-line)
  (:documentation "Provides utilities for 2D geometry using complex number.

Reference:
Christer Ericson. Real-Time Collision Detection. (I checked only a translation
to Japanese)"))
(in-package :cp/complex-geometry)

;; TODO: more docs and tests

;; Reference: https://www.geeksforgeeks.org/check-if-two-given-line-segments-intersect/
(declaim (inline intersect-p))
(defun intersect-p (p1 p2 q1 q2 &optional (eps 0))
  "Returns true iff the line segment from P1 to P2 intersects or overlaps with
the one from Q1 to Q2."
  (labels ((calc-orientation (p q r)
             (let ((val (- (* (- (imagpart q) (imagpart p))
                              (- (realpart r) (realpart q)))
                           (* (- (realpart q) (realpart p))
                              (- (imagpart r) (imagpart q))))))
               (cond ((> val eps) :clockwise)
                     ((< val (- eps)) :counterclockwise)
                     (t :collinear))))
           (on-segment-p (p q r)
             ;; FIXME: use EPS here too
             (and (<= (realpart q) (max (realpart p) (realpart r)))
                  (>= (realpart q) (min (realpart p) (realpart r)))
                  (<= (imagpart q) (max (imagpart p) (imagpart r)))
                  (>= (imagpart q) (min (imagpart p) (imagpart r))))))
    (let ((o1 (calc-orientation p1 p2 q1))
          (o2 (calc-orientation p1 p2 q2))
          (o3 (calc-orientation q1 q2 p1))
          (o4 (calc-orientation q1 q2 p2)))
      (or (not (or (eq o1 o2) (eq o3 o4)))
          (and (eq o1 :collinear) (on-segment-p p1 q1 p2))
          (and (eq o2 :collinear) (on-segment-p p1 q2 p2))
          (and (eq o3 :collinear) (on-segment-p q1 p1 q2))
          (and (eq o4 :collinear) (on-segment-p q1 p2 q2))))))

(declaim (inline calc-internal-angle)
         (ftype (function * (values double-float &optional))
                calc-internal-angle))
(defun calc-internal-angle (c1 c2)
  (acos (max -1d0 (min 1d0 (/ (+ (* (realpart c1) (realpart c2))
                                 (* (imagpart c1) (imagpart c2)))
                              (* (abs c1) (abs c2)))))))

(declaim (inline calc-angle))
(defun calc-angle (c1 c2)
  "Returns the anticlockwise angle (in the sense of amplitude) from vector C1 to
vector C2. The range is [0, 2PI).

NOTE: it is similar to CL:PHASE, the range of which is (-PI, PI]."
  (mod (- (phase c2) (phase c1)) #.(* 2 PI)))

(declaim (inline cross*))
(defun cross* (p1 p2)
  "Returns the `cross' product (a.k.a. perp-dot product) of two 2D vectors."
  (- (* (realpart p1) (imagpart p2))
     (* (imagpart p1) (realpart p2))))

(declaim (inline parallel-p))
(defun parallel-p (p1 p2 q1 q2 &optional (eps 0))
  (<= (abs (cross* (- p1 p2) (- q1 q2))) eps))

(declaim (inline dot-product))
(defun dot* (p1 p2)
  (+ (* (realpart p1) (realpart p2))
     (* (imagpart p1) (imagpart p2))))

(declaim (inline on-line-segment-p))
(defun on-line-segment-p (point end1 end2 &optional (eps 0))
  "Returns T iff POINT is on the line segment between END1 and END2."
  (and (<= (abs (cross* (- point end1) (- end2 end1))) eps)
       (or (<= (realpart end1) (realpart point) (realpart end2))
           (<= (realpart end2) (realpart point) (realpart end1)))
       (or (<= (imagpart end1) (imagpart point) (imagpart end2))
           (<= (imagpart end2) (imagpart point) (imagpart end1)))))

(declaim (inline overlap-p))
(defun overlap-p (p1 p2 q1 q2 &optional (eps 0))
  (and (parallel-p p1 p2 q1 q2 eps)
       (or (on-line-segment-p p1 q1 q2 eps)
           (on-line-segment-p p2 q1 q2 eps)
           (on-line-segment-p q1 p1 p2 eps)
           (on-line-segment-p q2 p1 p2 eps))))

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
    (dotimes (i n)
      (let* ((a (aref vector i))
             (b (aref vector (if (= i (- n 1)) 0 (+ i 1))))
             (a-to-b (- b a))
             (a-to-point (- point a))
             (current-side (signum (cross* a-to-b a-to-point))))
        (cond ((zerop current-side)
               (return-from inside-convex-polygon-p nil))
              ((null prev-side)
               (setq prev-side current-side))
              ((/= prev-side current-side)
               (return-from inside-convex-polygon-p nil)))))
    t))

(declaim (inline in-circle))
(defun in-circle (a b c point)
  "Returns the `degree' to which POINT is inside the circle given by three
points A, B, and C.

result > 0: inside the circle;
result < 0: outside the circle;
result = 0: on the circumference, or collinear."
  (declare (number a b c point))
  (let* ((m11 (- (realpart a) (realpart point)))
         (m12 (- (imagpart a) (imagpart point)))
         (m13 (+ (expt m11 2) (expt m12 2)))
         (m21 (- (realpart b) (realpart point)))
         (m22 (- (imagpart b) (imagpart point)))
         (m23 (+ (expt m21 2) (expt m22 2)))
         (m31 (- (realpart c) (realpart point)))
         (m32 (- (imagpart c) (imagpart point)))
         (m33 (+ (expt m31 2) (expt m32 2)))
         (det (- (+ (* m11 m22 m33)
                    (* m12 m23 m31)
                    (* m13 m21 m32))
                 (+ (* m13 m22 m31)
                    (* m12 m21 m33)
                    (* m11 m23 m32))))
         (cross (cross* (- b a) (- c a))))
    (* cross det)))

(declaim (inline barycentric))
(defun barycentric (a b c point)
  "Returns the barycentric coordinates of POINT w.r.t. A, B, and C."
  (declare (number a b c point))
  (let* ((v0 (- b a))
         (v1 (- c a))
         (v2 (- point a))
         (d00 (dot* v0 v0))
         (d01 (dot* v0 v1))
         (d11 (dot* v1 v1))
         (d20 (dot* v2 v0))
         (d21 (dot* v2 v1))
         (denom (- (* d00 d11) (* d01 d01)))
         (coord-b (/ (- (* d11 d20) (* d01 d21)) denom))
         (coord-c (/ (- (* d00 d21) (* d01 d20)) denom)))
    (values (- 1 coord-b coord-c) coord-b coord-c)))

(declaim (inline convex-quad-p))
(defun convex-quad-p (a b c d)
  "Returns true iff the quadrilateral ABCD (which can be clockwise or
counterclowise) is convex."
  (declare (number a b c d))
  (and (let ((bda (cross* (- d b) (- a b)))
             (bdc (cross* (- d b) (- c b))))
         (< (dot* bda bdc) 0))
       (let ((acd (cross* (- c a) (- d a)))
             (acb (cross* (- c a) (- b a))))
         (< (dot* acd acb) 0))))

(declaim (inline point<))
(defun point< (p1 p2)
  "Lexicographically compares two points."
  (if (= (realpart p1) (realpart p2))
      (< (imagpart p1) (imagpart p2))
      (< (realpart p1) (realpart p2))))

(defun canonize-line (p1 p2)
  "Returns a list of three integers that characterizes the line that crosses P1
and P2. P1 and P2 must be grid points."
  (assert (/= p1 p2))
  (let* ((dy (- (imagpart p2) (imagpart p1)))
         (dx (- (realpart p2) (realpart p1)))
         (c (- (* (imagpart p1) dx) (* (realpart p1) dy)))
         (gcd (gcd dy dx c))
         (a (floor dy gcd))
         (b (floor (- dx) gcd))
         (c (floor c gcd)))
    (if (or (< a 0)
            (and (zerop a) (< b)))
        (list (- a) (- b) (- c))
        (list a b c))))

(declaim (inline merge-segments))
(defun merge-segments (sorted-segments)
  (let (stack)
    (dolist (segment sorted-segments)
      (destructuring-bind (p1 . p2) segment
        (if (or (null stack)
                (point< (cdr (car stack)) p1))
            (push segment stack)
            (when (point< (cdr (car stack)) p2)
              (let ((new-segment (cons (car (car stack)) p2)))
                (pop stack)
                (push new-segment stack))))))
    (nreverse stack)))

(defun calc-intersection (line1 line2)
  (destructuring-bind (a1 b1 c1) line1
    (destructuring-bind (a2 b2 c2) line2
      (let ((det (- (* a1 b2) (* b1 a2))))
        (unless (zerop det)
          (cons (/ (- (* b1 c2) (* b2 c1)) det)
                (/ (- (* a2 c1) (* a1 c2)) det)))))))
