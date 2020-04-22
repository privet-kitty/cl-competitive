;;;;
;;;; Sliding window optimum
;;;;


;;;
;;; For window of fixed width
;;;

(declaim (inline calc-sliding-opt))
(defun calc-sliding-opt (vector width order)
  "Computes all the minima (or maxima) of the subsequences of the given width,
VECTOR[i, i+WIDTH), in O(n). Returns a vector, whose i-th element is the
minimum (or maximum) of the subsequence beginning with the index i.

ORDER := strict order (#'< corresponds to slide min. and #'> to slide max.)"
  (declare (vector vector)
           ((integer 1 #.most-positive-fixnum) width))
  (let* ((n (length vector))
         (deq (make-array n :element-type '(integer 0 #.most-positive-fixnum)))
         (front-pos 0)
         (end-pos -1)
         (l 0)
         (r 0)
         (res (make-array (+ 1 (- n width)) :element-type (array-element-type vector))))
    (declare ((integer -1 #.most-positive-fixnum) front-pos end-pos l r))
    (assert (<= width n))
    (labels ((push-back (x)
               (incf end-pos)
               (setf (aref deq end-pos) x))
             (pop-back ()
               (decf end-pos))
             (pop-front ()
               (incf front-pos))
             (peek-back ()
               (aref deq end-pos))
             (peek-front ()
               (aref deq front-pos))
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

;;;
;;; For window of variable width
;;;

(defstruct (sliding-window (:constructor make-sliding-window
                               (size &aux
                                     (times (make-array size :element-type 'fixnum))
                                     (values (make-array size :element-type 'fixnum))))
                           (:conc-name %swindow-)
                           (:copier nil))
  (front-pos 0 :type (integer 0 #.most-positive-fixnum))
  (end-pos -1 :type (integer -1 #.most-positive-fixnum))
  (times nil :type (simple-array fixnum (*)))
  (values nil :type (simple-array fixnum (*))))

(defun %swindow-push-back (time value sw)
  (let ((new-end-pos (+ 1 (%swindow-end-pos sw))))
    (setf (aref (%swindow-times sw) new-end-pos) time
          (aref (%swindow-values sw) new-end-pos) value
          (%swindow-end-pos sw) new-end-pos)))

(defun %swindow-pop-back (sw)
  (decf (%swindow-end-pos sw)))

(defun %swindow-pop-front (sw)
  (incf (%swindow-front-pos sw)))

(declaim (inline swindow-extend))
(defun swindow-extend (time value sw order)
  "ORDER := #'< => minimum
ORDER := #'> => maximum"
  (let ((values (%swindow-values sw)))
    (loop while (and (<= (%swindow-front-pos sw) (%swindow-end-pos sw))
                     (not (funcall order
                                   (aref values (%swindow-end-pos sw))
                                   value)))
          do (%swindow-pop-back sw))
    (%swindow-push-back time value sw)))

(declaim (inline swindow-shrink))
(defun swindow-shrink (time sw)
  "Advance the left end of the time range to TIME (inclusive)."
  (let ((times (%swindow-times sw)))
    (loop while (and (<= (%swindow-front-pos sw) (%swindow-end-pos sw))
                     (< (aref times (%swindow-front-pos sw)) time))
          do (%swindow-pop-front sw))))

(declaim (inline swindow-empty-p))
(defun swindow-empty-p (sw)
  (> (%swindow-front-pos sw) (%swindow-end-pos sw)))

(declaim (inline swindow-get-opt))
(defun swindow-get-opt (sw)
  (assert (not (swindow-empty-p sw)))
  (let ((front-pos (%swindow-front-pos sw)))
    (aref (%swindow-values sw) front-pos)))

(declaim (inline swindow-reinitialize))
(defun swindow-reinitialize (sw)
  (setf (%swindow-front-pos sw) 0
        (%swindow-end-pos sw) -1))
