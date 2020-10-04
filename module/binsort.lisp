;;;
;;; Bucket sort and counting sort
;;;

(defpackage :cp/binsort
  (:use :cl)
  (:export #:map-binsorted #:do-binsorted #:binsort!))
(in-package :cp/binsort)

(declaim (inline map-binsorted))
(defun map-binsorted (function sequence range-max &key from-end key)
  "Calls FUNCTION with each ascending non-negative integer in SEQUENCE if KEY is
null. If KEY is non-nil, this function calls FUNCTION with each element of
SEQUENCE in the order of the (non-negative) integers that (FUNCALL KEY
<element>) returns. Any of these integers must not exceed RANGE-MAX. If FROM-END
is true, the descending order is adopted instead. This function is
non-destructive."
  (declare ((mod #.array-total-size-limit) range-max))
  (if key
      (let ((buckets (make-array (1+ range-max) :element-type 'list :initial-element nil))
            (existing-min most-positive-fixnum)
            (existing-max 0))
        (declare ((integer 0 #.most-positive-fixnum) existing-min existing-max))
        (sequence:dosequence (e sequence)
          (let ((value (funcall key e)))
            (push e (aref buckets value))
            (setf existing-min (min value existing-min))
            (setf existing-max (max value existing-max))))
        (if from-end
            (loop for x from existing-max downto existing-min
                  do (dolist (e (aref buckets x))
                       (funcall function e)))
            (loop for x from existing-min to existing-max
                  do (dolist (e (aref buckets x))
                       (funcall function e)))))
      ;; If KEY is not given, all we need is counting sort.
      (let ((counts (make-array (1+ range-max) :element-type 'fixnum :initial-element 0))
            (existing-min most-positive-fixnum)
            (existing-max 0))
        (declare ((integer 0 #.most-positive-fixnum) existing-min existing-max))
        (sequence:dosequence (e sequence)
          (incf (aref counts e))
          (setf existing-min (min e existing-min))
          (setf existing-max (max e existing-max)))
        (if from-end
            (loop for x from existing-max downto existing-min
                  do (loop repeat (aref counts x)
                           do (funcall function x)))
            (loop for x from existing-min to existing-max
                  do (loop repeat (aref counts x)
                           do (funcall function x)))))))

(defmacro do-binsorted ((var sequence range-max &key from-end key finally) &body body)
  "DO-style macro for MAP-BINSORTED"
  `(block nil
     (map-binsorted (lambda (,var) ,@body) ,sequence ,range-max
                    :from-end ,from-end :key ,key)
     ,finally))

;; not tested
(declaim (inline binsort!))
(defun binsort! (sequence range-max &key from-end key)
  (declare ((mod #.array-total-size-limit) range-max))
  (if key
      (let ((buckets (make-array (1+ range-max) :element-type 'list :initial-element nil)))
        (sequence:dosequence (e sequence)
          (push e (aref buckets (funcall key e))))
        (let ((pos 0))
          (declare ((integer 0 #.most-positive-fixnum) pos))
          (if from-end
              (loop for x from range-max downto 0
                    do (dolist (e (aref buckets x))
                         (setf (aref sequence pos) e)
                         (incf pos)))
              (loop for x from 0 to range-max
                    do (dolist (e (aref buckets x))
                         (setf (aref sequence pos) e)
                         (incf pos))))))
      ;; If KEY is not given, all we need is counting sort.
      (let ((counts (make-array (1+ range-max)
                                :element-type '(integer 0 #.most-positive-fixnum)
                                :initial-element 0)))
        (sequence:dosequence (e sequence)
          (incf (aref counts e)))
        (let ((pos 0))
          (declare ((integer 0 #.most-positive-fixnum) pos))
          (if from-end
              (loop for x from range-max downto 0
                    do (loop repeat (aref counts x)
                             do (setf (aref sequence pos) x)
                                (incf pos)))
              (loop for x from 0 to range-max
                    do (loop repeat (aref counts x)
                             do (setf (aref sequence pos) x)
                                (incf pos)))))))
  sequence)
