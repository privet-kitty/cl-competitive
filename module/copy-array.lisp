(defpackage :cp/copy-array
  (:use :cl)
  (:import-from #:sb-c #:defknown #:flushable
                #:defoptimizer #:derive-type #:unsupplied-or-nil #:lvar-type
                #:compiler-warn #:combination-kind #:*compiler-error-context*)
  (:import-from #:sb-kernel #:type-specifier #:type-intersection #:make-array-type
                #:*wild-type* #:*empty-type* #:array-type-p)
  (:import-from #:sb-int #:index)
  (:export #:copy-array))
(in-package :cp/copy-array)

#+sbcl
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defknown copy-array (array)
      array (flushable)
    :overwrite-fndb-silently t)
  (defoptimizer (copy-array derive-type)
      ((array)
       node)
    (let* ((array-type (lvar-type array))
           (int (type-intersection array-type
                                   (make-array-type '* :element-type *wild-type*))))
      (if (array-type-p int)
          (make-array-type (sb-c::array-type-dimensions int)
                           :complexp nil
                           :element-type (sb-c::array-type-element-type int)
                           :specialized-element-type (sb-c::array-type-specialized-element-type int))
          (make-array-type '* :element-type *wild-type*)))))

(defun copy-array (array)
  "Returns a (simple) copy of an ARRAY.

If the array is a vector with a fill pointer, the fill pointer is ignored and
the whole array is copied."
  (declare (array array))
  (let ((new (make-array (array-dimensions array) :element-type (array-element-type array))))
    #+sbcl
    (when (typep array 'simple-array)
      (replace (sb-ext:array-storage-vector new) (sb-ext:array-storage-vector array))
      (return-from copy-array new))
    (dotimes (i (array-total-size array))
      (setf (row-major-aref new i) (row-major-aref array i)))
    new))
