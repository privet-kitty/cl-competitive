(defpackage :cp/copy-array
  (:use :cl)
  (:import-from #:sb-c #:defknown #:flushable
                #:defoptimizer #:derive-type #:unsupplied-or-nil #:lvar-type
                #:compiler-warn #:combination-kind #:*compiler-error-context*)
  (:import-from #:sb-kernel #:type-specifier #:type-intersection #:make-array-type
                #:*wild-type* #:*empty-type*)
  (:import-from #:sb-int #:index)
  (:export #:copy-array))
(in-package :cp/copy-array)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defknown copy-array (array)
      array (flushable)
    :overwrite-fndb-silently t)
  (defoptimizer (copy-array derive-type)
      ((array)
       node)
    (let* ((array-type (lvar-type array))
           (int (type-intersection array-type
                                   (make-array-type '* 
                                                    :complexp nil
                                                    :element-type *wild-type*))))
      (if (eq int *empty-type*)
          (let ((*compiler-error-context* node))
            (setf (combination-kind node) :error)
            (compiler-warn "~A doesn't seem to be a SIMPLE-ARRAY"
                           (type-specifier array-type)))
          int))))

(defun copy-array (array)
  "Returns a copy of an ARRAY.

NOTE: Currently this function can only deal with a simple-array."
  (declare (simple-array array))
  (assert (and (null (array-displacement array))
               (not (array-has-fill-pointer-p array))))
  (let ((new (make-array (array-dimensions array) :element-type (array-element-type array))))
    (replace (sb-ext:array-storage-vector new) (sb-ext:array-storage-vector array))
    new))
