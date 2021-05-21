(defpackage :cp/sliding-window
  (:use :cl)
  (:export #:sliding-window #:make-sliding-window
           #:swindow-extend #:swindow-shrink #:swindow-fold #:swindow-empty-p #:swindow-clear
           #:sliding-window-function #:make-swf #:swf-fold
           #:swf-extend #:swf-shrink)
  (:documentation "Provides sliding window minimum/maximum."))
(in-package :cp/sliding-window)

(defstruct (sliding-window (:constructor make-sliding-window
                               (size &aux
                                     (times (make-array size :element-type 'fixnum))
                                     (values (make-array size :element-type 'fixnum))))
                           (:conc-name %swindow-)
                           (:copier nil)
                           (:predicate nil))
  (front-pos 0 :type (mod #.array-dimension-limit))
  (end-pos 0 :type (mod #.array-dimension-limit))
  (times nil :type (simple-array fixnum (*)))
  (values nil :type (simple-array fixnum (*))))

(declaim (inline %swindow-push-back))
(defun %swindow-push-back (time value sw)
  (let ((end-pos (%swindow-end-pos sw)))
    (setf (aref (%swindow-times sw) end-pos) time
          (aref (%swindow-values sw) end-pos) value
          (%swindow-end-pos sw) (+ 1 end-pos))))

(declaim (inline %swindow-pop-back))
(defun %swindow-pop-back (sw)
  (decf (%swindow-end-pos sw)))

(declaim (inline %swindow-pop-front))
(defun %swindow-pop-front (sw)
  (incf (%swindow-front-pos sw)))

(declaim (inline swindow-empty-p))
(defun swindow-empty-p (sw)
  (= (%swindow-front-pos sw) (%swindow-end-pos sw)))

(declaim (inline swindow-extend))
(defun swindow-extend (time value sw order)
  "ORDER := #'< => minimum
ORDER := #'> => maximum"
  (let ((values (%swindow-values sw)))
    (loop while (and (not (swindow-empty-p sw))
                     (funcall order value (aref values (- (%swindow-end-pos sw) 1))))
          do (%swindow-pop-back sw))
    (%swindow-push-back time value sw)))

(declaim (inline swindow-shrink))
(defun swindow-shrink (time sw)
  "Advance the left end of the time range to TIME (inclusive)."
  (let ((times (%swindow-times sw)))
    (loop while (and (not (swindow-empty-p sw))
                     (< (aref times (%swindow-front-pos sw)) time))
          do (%swindow-pop-front sw))))

(declaim (inline swindow-fold))
(defun swindow-fold (sw)
  (assert (not (swindow-empty-p sw)))
  (aref (%swindow-values sw) (%swindow-front-pos sw)))

(declaim (inline swindow-clear))
(defun swindow-clear (sw)
  (setf (%swindow-front-pos sw) 0
        (%swindow-end-pos sw) 0))

(defstruct (sliding-window-function
            (:constructor make-swf
                (function order length
                 &key (start 0)
                 &aux (swindow (make-sliding-window length))
                      (left start)
                      (right start)))
            (:conc-name %swf-)
            (:copier nil)
            (:predicate nil))
  (swindow nil :type sliding-window)
  (function nil :type function)
  (order nil :type function)
  (left 0 :type (mod #.array-dimension-limit))
  (right 0 :type (mod #.array-dimension-limit)))

(declaim (inline swf-fold))
(defun swf-fold (swf &optional l r)
  "ORDER := #'< => minimum
ORDER := #'> => maximum"
  (declare ((or null (mod #.array-dimension-limit)) l r))
  (let ((sw (%swf-swindow swf))
        (function (%swf-function swf)))
    (symbol-macrolet ((left (%swf-left swf))
                      (right (%swf-right swf)))
      (let ((l (or l left))
            (r (or r right)))
        (assert (and (<= left l) (<= right r) (< l r)))
        (loop while (< right r)
              do (swindow-extend right (funcall function right) sw (%swf-order swf))
                 (incf right))
        (setq left l)
        (swindow-shrink l sw)))
    (swindow-fold sw)))

(declaim (inline swf-extend))
(defun swf-extend (swf)
  (swindow-extend (%swf-right swf)
                  (funcall (%swf-function swf) (%swf-right swf))
                  (%swf-swindow swf)
                  (%swf-order swf))
  (incf (%swf-right swf)))

(declaim (inline swf-shrink))
(defun swf-shrink (swf)
  (assert (<= (%swf-left swf) (%swf-right swf)))
  (incf (%swf-left swf))
  (swindow-shrink (%swf-left swf) (%swf-swindow swf)))
