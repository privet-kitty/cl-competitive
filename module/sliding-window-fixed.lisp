(defpackage :cp/sliding-window-fixed
  (:use :cl)
  (:export #:sliding-window-fixed))
(in-package :cp/sliding-window-fixed)

(declaim (inline sliding-window-fixed))
(defun sliding-window-fixed (vector width order)
  "Computes the minima (or maxima) of all the subarray of the given
width (VECTOR[i, i+WIDTH)) in O(n) time. Returns a vector, whose i-th element is
the minimum (or maximum) of the array beginning with the index i.

ORDER := strict order (#'< corresponds to slide min. and #'> to slide max.)"
  (declare (vector vector)
           ((integer 1 #.array-dimension-limit) width))
  (let* ((n (length vector))
         (deq (make-array n :element-type '(integer 0 #.most-positive-fixnum)))
         (front-pos 0)
         (end-pos -1)
         (l 0)
         (r 0)
         (res (make-array (+ 1 (- n width)) :element-type (array-element-type vector))))
    (declare ((integer -1 #.array-dimension-limit) front-pos end-pos l r))
    (assert (<= width n))
    (labels ((push-back (x)
               (incf end-pos)
               (setf (aref deq end-pos) x))
             (pop-back () (decf end-pos))
             (pop-front () (incf front-pos))
             (peek-back () (aref deq end-pos))
             (peek-front () (aref deq front-pos))
             (extend ()
               (loop while (and (<= front-pos end-pos)
                                (not (funcall order
                                              (aref vector (peek-back))
                                              (aref vector r))))
                     do (pop-back))
               (push-back r)
               (incf r))
             (shrink ()
               (when (= (aref deq front-pos) l)
                 (pop-front))
               (incf l)))
      (declare (inline push-back pop-back pop-front peek-back peek-front))
      (loop for i below width
            do (extend))
      (loop for i from width below n
            do (setf (aref res (- i width))
                     (aref vector (peek-front)))
               (extend)
               (shrink)
            finally (setf (aref res (- i width))
                          (aref vector (peek-front))))
      res)))
