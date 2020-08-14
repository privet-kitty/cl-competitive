;;;
;;; Welzl's algorithm for smallest circle problem (not optimized)
;;;
;;; Reference:
;;; Mark de Berg et al., Computational Geometry: Algorithms and Applications, 3rd Edition
;;;

(defpackage :cp/welzl
  (:use :cl :cp/circumcenter :cp/shuffle)
  (:export #:calc-smallest-circle))
(in-package :cp/welzl)

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

(declaim (inline %mini-disc-with-point))
(defun %mini-disc-with-point (points end q eps)
  (declare (vector points)
           ((integer 0 #.most-positive-fixnum) end))
  (shuffle! points 0 end)
  (let* ((center (* 1/2 (+ (aref points 0) q)))
         (radius (abs (coerce (- q center) '(complex double-float)))))
    (loop for i from 1 below end
          for new-point = (aref points i)
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
  (let* ((points (copy-seq (shuffle! points)))
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
