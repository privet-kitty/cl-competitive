(defpackage :cp/mex-manager
  (:use :cl)
  (:export #:make-mex-manager #:mex-get #:mex-add)
  (:documentation "Provides a data structure to deal with MEX in constant time.

add a new number: amortized O(1);
get MEX: O(1)."))
(in-package :cp/mex-manager)

(declaim (inline %power-of-two-ceiling))
(defun %power-of-two-ceiling (x)
  (ash 1 (integer-length (- x 1))))

(defconstant +default-size+ 64)
(defstruct (mex-manager
            (:constructor make-mex-manager
                (&optional (size +default-size+)
                 &aux
                 (marked (make-array (%power-of-two-ceiling (max 1 size))
                                     :element-type 'bit
                                     :initial-element 0))))
            (:copier nil)
            (:predicate nil)
            (:conc-name %mex-))
  (current 0 :type (mod #.array-dimension-limit))
  (marked nil :type simple-bit-vector)
  (unprocessed nil :type list))

(declaim (inline mex-get))
(defun mex-get (mex-manager)
  (%mex-current mex-manager))

(defun %mex-extend (mex-manager new-size)
  (declare (optimize (speed 3))
           ((mod #.array-dimension-limit) new-size))
  (symbol-macrolet ((marked (%mex-marked mex-manager))
                    (unprocessed (%mex-unprocessed mex-manager))
                    (current (%mex-current mex-manager)))
    (setq marked (adjust-array marked new-size)
          unprocessed (loop ;; FIXME: more efficient operation is possible.
                            for x of-type (mod #.array-dimension-limit) in unprocessed
                            when (< x new-size)
                            do (setf (aref marked x) 1)
                            else
                            collect x))))

(defun %mex-forward (mex-manager)
  (declare (optimize (speed 3)))
  (symbol-macrolet ((marked (%mex-marked mex-manager))
                    (current (%mex-current mex-manager))
                    (size (length marked)))
    (loop until (zerop (aref marked current))
          do (loop while (and (< current size)
                              (= 1 (aref marked current)))
                   do (incf current))
             (when (= current size)
               (%mex-extend mex-manager (* 2 size))))))

(defun mex-add (mex-manager number)
  (declare (optimize (speed 3))
           ((mod #.array-dimension-limit) number))
  (symbol-macrolet ((marked (%mex-marked mex-manager))
                    (unprocessed (%mex-unprocessed mex-manager)))
    (if (< number (length marked))
        (setf (aref marked number) 1)
        (push number unprocessed))
    (%mex-forward mex-manager)
    mex-manager))
