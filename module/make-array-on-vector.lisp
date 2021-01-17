(defpackage :cp/make-array-on-vector
  (:use :cl)
  (:export #:make-array-on-vector))
(in-package :cp/make-array-on-vector)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (sb-c:defknown make-array-on-vector ((simple-array * (*)) list)
      simple-array (sb-c:flushable)
    :overwrite-fndb-silently t)
  ;; (sb-c:defoptimizer (make-array-on-vector sb-c:derive-type) ((vector dims) node)
  ;;   (block nil
  ;;     (let* ((vector-type (sb-c::lvar-type vector))
  ;;            (element-type (when (sb-c::array-type-p vector-type)
  ;;                            (sb-c::array-type-element-type vector-type)))
  ;;            (spec `(simple-array
  ;;                    ,(if (or (null element-type) (sb-c::contains-unknown-type-p element-type))
  ;;                         '*
  ;;                         (sb-c::type-specifier element-type))
  ;;                    ,(if (sb-c::constant-lvar-p dims)
  ;;                         (let ((constant-dims (sb-c::lvar-value dims)))
  ;;                           (unless (sb-c::check-array-dimensions constant-dims node)
  ;;                             (return))
  ;;                           constant-dims)
  ;;                         '*))))
  ;;       (sb-c::careful-specifier-type spec))))
  )

(defun make-array-on-vector (vector dimensions)
  "Returns a multi-dimensional array that uses VECTOR as its storage. The
product of DIMENSIONS must be equal to the length of VECTOR."
  (declare (optimize (speed 3))
           ((simple-array * (*)) vector)
           (list dimensions))
  (assert (= (the fixnum (reduce #'* dimensions)) (length vector)))
  (let* ((array-rank (length dimensions))
         (array (sb-kernel::make-array-header sb-vm::simple-array-widetag array-rank)))
    (declare ((integer 2 (#.array-rank-limit)) array-rank))
    (setf (sb-kernel:%array-available-elements array) (length vector)
          (sb-kernel:%array-displaced-from array) nil
          (sb-kernel:%array-displaced-p array) nil)
    (setf (#.(or (find-symbol "%ARRAY-DATA" :sb-kernel)
                 ;; for SBCL version earlier than 1.3.19 
                 (find-symbol "%ARRAY-DATA-VECTOR" :sb-kernel))
             array)
          vector)
    (dotimes (axis array-rank)
      (setf (sb-kernel:%array-dimension array axis) (pop dimensions)))
    array))
