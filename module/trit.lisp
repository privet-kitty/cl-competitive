(defpackage :cp/trit
  (:use :cl)
  (:import-from :sb-c #:define-source-transform #:defknown #:defoptimizer #:deftransform
                #:foldable #:flushable
                #:lvar-type #:specifier-type #:numeric-type-high #:numeric-type-p
                #:csubtypep #:constant-lvar-p #:lvar-value)
  (:export #:logtrit #:ldt))
(in-package :cp/trit)

(deftype uint () '(integer 0 #.most-positive-fixnum))
(defconstant +n-word-trits+ (loop for exp from 0
                                  while (<= (expt 3 exp) most-positive-fixnum)
                                  finally (return (- exp 1))))
(deftype unsigned-trit (&optional (size +n-word-trits+)) `(mod ,(expt 3 size)))

(declaim ((simple-array uint (*)) *power3*))
(sb-ext:define-load-time-global
    *power3* (make-array (+ 1 +n-word-trits+) :element-type 'uint))
(dotimes (i (length *power3*))
  (setf (aref *power3* i) (expt 3 i)))

(declaim (inline logtrit)
         (ftype (function * (values (mod 3) &optional)) logtrit))
(defun logtrit (index integer)
  (declare ((mod #.+n-word-trits+) index)
           (unsigned-trit integer))
  (nth-value 0 (floor (mod integer (aref *power3* (+ index 1)))
                      (aref *power3* index))))

(declaim (ftype (function * (values unsigned-trit &optional)) %trit-set))
(defun %trit-set (new-value index integer)
  (declare (optimize (speed 3))
           ((mod 3) new-value)
           ((mod #.+n-word-trits+) index)
           (unsigned-trit integer))
  (multiple-value-bind (quot rem) (floor integer (aref *power3* index))
    (let ((current (mod quot 3)))
      (declare ((mod 3) current))
      (+ (the unsigned-trit (* (+ quot (- new-value current))
                               (aref *power3* index)))
         rem))))

(define-setf-expander logtrit (index integer &environment env)
  (multiple-value-bind (tmps vals stores store-form access-form)
      (get-setf-expansion integer env)
    (when (cdr stores)
      (error "SETF LOGTRIT too hairy."))
    (let ((index-tmp (gensym "INDEX"))
          (store (gensym "NEW")))
      (values (cons index-tmp tmps)
              (cons index vals)
              (list store)
              `(let ((,(car stores) (%trit-set ,store ,index-tmp ,access-form)))
                 ,store-form
                 ,store)
              `(logtrit ,index-tmp ,access-form)))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defknown ldt (cons uint) uint (foldable flushable)
    :overwrite-fndb-silently t)
  (defknown %ldt (uint uint uint) uint (foldable flushable)
    :overwrite-fndb-silently t))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defoptimizer (%ldt derive-type) ((size posn num))
    (declare (ignore posn num))
    (let ((size (lvar-type size)))
      (when (and (numeric-type-p size)
                 (csubtypep size (specifier-type 'integer)))
        (let ((size-high (numeric-type-high size)))
          (if (and size-high (<= size-high +n-word-trits+))
              (specifier-type `(unsigned-trit ,size-high))
              (specifier-type 'unsigned-byte))))))

  (deftransform %ldt ((size posn integer) (uint uint uint) uint)
    "convert to inline logical operations"
    ;; TODO: constant-folding
    `(nth-value 0 (floor (mod integer (aref *power3* (+ posn size)))
                         (aref *power3* posn)))))

(defun %ldt (size posn integer)
  (declare (optimize (speed 3))
           (uint size posn integer))
  (%ldt size posn integer))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (locally (declare (sb-ext:muffle-conditions warning))
    (define-source-transform ldt (spec int &environment env)
      (block ldt ; source-transform is not enclosed with a block of the same
                 ; name before SBCL 1.3.8.
        (labels ((pass () (return-from ldt (values nil t))))
          (let ((spec (handler-case (sb-int:%macroexpand spec env)
                        (error () (pass)))))
            (if (and (consp spec) (eq (car spec) 'byte))
                (if (sb-int:proper-list-of-length-p (cdr spec) 2)
                    (values `(%ldt ,(second spec) ,(third spec) ,int) nil)
                    (pass))
                (let ((byte (copy-symbol 'byte)))
                  (values `(let ((,byte ,spec))
                             (%ldt (byte-size ,byte) (byte-position ,byte) ,int))
                          nil)))))))))

(defun ldt (bytespec integer)
  (ldt bytespec integer))
