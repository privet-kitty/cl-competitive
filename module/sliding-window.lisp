(defpackage :cp/sliding-window
  (:use :cl)
  (:export #:sliding-window #:make-sliding-window
           #:swindow-extend #:swindow-shrink #:swindow-get #:swindow-empty-p #:swindow-clear)
  (:documentation "Provides sliding window minimum/maximum."))
(in-package :cp/sliding-window)

(defstruct (sliding-window (:constructor make-sliding-window
                               (size &aux
                                     (times (make-array size :element-type 'fixnum))
                                     (values (make-array size :element-type 'fixnum))))
                           (:conc-name %swindow-)
                           (:copier nil)
                           (:predicate nil))
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
(defun swindow-get (sw)
  (assert (not (swindow-empty-p sw)))
  (let ((front-pos (%swindow-front-pos sw)))
    (aref (%swindow-values sw) front-pos)))

(declaim (inline swindow-clear))
(defun swindow-clear (sw)
  (setf (%swindow-front-pos sw) 0
        (%swindow-end-pos sw) -1))
