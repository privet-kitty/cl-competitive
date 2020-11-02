;;;
;;; Utilities for 2D geometry (using complex number)
;;;

(defpackage :cp/complex-geometry
  (:use :cl)
  (:export #:intersect-p #:calc-internal-angle #:calc-angle #:on-line-segment-p
           #:cross* #:dot* #:inside-convex-polygon-p))
(in-package :cp/complex-geometry)

;; not tested
;; Reference: https://www.geeksforgeeks.org/check-if-two-given-line-segments-intersect/
(declaim (inline intersect-p))
(defun intersect-p (p1 p2 q1 q2 &optional (eps 0))
  "Returns true iff the line segment from P1 to P2 intersects or overwraps with
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
(defun cross* (p1 p2)
  (- (* (realpart p1) (imagpart p2))
     (* (imagpart p1) (realpart p2))))

(declaim (inline dot-product))
(defun dot* (p1 p2)
  (+ (* (realpart p1) (realpart p2))
     (* (imagpart p1) (imagpart p2))))

(declaim (inline on-line-segment-p))
(defun on-line-segment-p (point end1 end2 &optional (eps 0))
  "Returns T iff POINT is on the line segment between END1 and END2."
  (and (<= (abs (cross-product (- point end1) (- end2 end1))) eps)
       (or (<= (realpart end1) (realpart point) (realpart end2))
           (<= (realpart end2) (realpart point) (realpart end1)))
       (or (<= (imagpart end1) (imagpart point) (imagpart end2))
           (<= (imagpart end2) (imagpart point) (imagpart end1)))))

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
