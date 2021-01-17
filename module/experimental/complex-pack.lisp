(eval-when (:compile-toplevel :load-toplevel :execute)
  (when (find-symbol "DOUBLE-FLOAT-BITS" :sb-kernel)
    (pushnew :double-float-bits *features*)))

(defpackage :cp/experimental/complex-pack
  (:use :cl)
  (:export #:define-complex-pack)
  (:import-from :sb-kernel
                #:make-double-float
                #+double-float-bits #:double-float-bits))
(in-package :cp/experimental/complex-pack)

;; TODO: unify define-cons-pack and define-complex-pack

#-double-float-bits
(eval-when (:compile-toplevel :load-toplevel :execute)
  (sb-c:defknown double-float-bits (double-float)
      ;; KLUDGE: actually return-type of sb-kernel:double-float-bits is
      ;; (signed-byte 64)
      (unsigned-byte 64)
      (sb-c:movable sb-c:foldable sb-c:flushable)
    :overwrite-fndb-silently t)
  (declaim (inline double-float-bits))
  (defun double-float-bits (x)
    (dpb (sb-kernel::double-float-high-bits x)
         (byte 32 32)
         (sb-kernel::double-float-low-bits x))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun %concat-name (&rest args)
    (if (cdr args)
        (format nil "~A-~A"
                (car args)
                (apply #'%concat-name (cdr args)))
        (car args)))
  (defun %concat+name+ (&rest args)
    (format nil "+~A+" (apply #'%concat-name args)))
  (defun extract-62bit-slots (list)
    (let ((position 0))
      (loop for (slot-name slot-size) in list
            while (<= (+ position slot-size) 62)
            collect (list slot-name slot-size position)
            do (incf position slot-size)))))

(defmacro define-complex-pack (name &rest slot-descriptions)
  (assert slot-descriptions () "~A has no slots." name)
  (let* ((packer-name (intern (%concat-name "PACK" name)))
         (unpacker-macro-name (intern (%concat-name "WITH-UNPACKING" name)))
         (tmp1 (gensym))
         (tmp2 (gensym))
         (tmp (gensym))
         ;; slot := (slot-name slot-size position)
         (real-slots (extract-62bit-slots slot-descriptions))
         (real-revslots (reverse real-slots))
         (imag-slots (extract-62bit-slots (nthcdr (length real-slots) slot-descriptions)))
         (imag-revslots (reverse imag-slots))
         (slots (append real-slots imag-slots)))
    (assert (= (+ (length real-slots) (length imag-slots))
               (length slot-descriptions))
            () "Size restriction violated: each cell <= 62 bit, total size <= 124 bit")
    (unless (> (length imag-slots) 0)
      (error "Whole size is too small. Use DEFINE-INTEGER-PACK instead."))
    (let ((real-width (+ (second (first real-revslots))
                         (third (first real-revslots))))
          (imag-width (+ (second (first imag-revslots))
                         (third (first imag-revslots)))))
      `(progn
         (deftype ,name () '(complex double-float))
         ;; define most positive integer for every slot as constant
         ,@(loop for (slot-name slot-size _) in slots
                 collect `(defconstant ,(intern (%concat+name+ "MAX" name slot-name))
                            (- (ash 1 ,slot-size) 1)))
         ;; getter
         ;; TODO: define setter with define-setf-expander
         ,@(loop for slot in real-slots
                 for (slot-name slot-size slot-position) = slot
                 for accessor-name = (intern (%concat-name name slot-name))
                 append `((declaim (inline ,accessor-name (setf ,accessor-name)))
                          (defun ,accessor-name (,name)
                            (declare (type ,name ,name))
                            (ldb (byte ,slot-size ,slot-position)
                                 (the (unsigned-byte ,real-width)
                                      (double-float-bits (realpart ,name)))))))
         ,@(loop for slot in imag-slots
                 for (slot-name slot-size slot-position) = slot
                 for accessor-name = (intern (%concat-name name slot-name))
                 append `((declaim (inline ,accessor-name (setf ,accessor-name)))
                          (defun ,accessor-name (,name)
                            (declare (type ,name ,name))
                            (ldb (byte ,slot-size ,slot-position)
                                 (the (unsigned-byte ,imag-width)
                                      (double-float-bits (imagpart ,name)))))))
         ;; constructor
         (declaim (inline ,packer-name))
         (defun ,packer-name ,(loop for slot in slots collect (car slot))
           (declare ,@(loop for (slot-name slot-size slot-position) in slots
                            collect `(type (unsigned-byte ,slot-size) ,slot-name)))
           (let ((,tmp1 ,(caar real-revslots))
                 (,tmp2 ,(caar imag-revslots)))
             (declare (type (unsigned-byte ,real-width) ,tmp1)
                      (type (unsigned-byte ,imag-width) ,tmp2))
             ,@(loop for (slot-name slot-size _) in (rest real-revslots)
                     collect `(setq ,tmp1 (logxor ,slot-name
                                                  (the (unsigned-byte ,real-width)
                                                       (ash ,tmp1 ,slot-size)))))
             ,@(loop for (slot-name slot-size _) in (rest imag-revslots)
                     collect `(setq ,tmp2 (logxor ,slot-name
                                                  (the (unsigned-byte ,imag-width)
                                                       (ash ,tmp2 ,slot-size)))))
             (complex (make-double-float (ldb (byte 30 32) ,tmp1)
                                         (ldb (byte 32 0) ,tmp1))
                      (make-double-float (ldb (byte 30 32) ,tmp2)
                                         (ldb (byte 32 0) ,tmp2)))))
         ;; destructuring-bind-style macro
         (defmacro ,unpacker-macro-name (vars ,name &body body)
           (check-type vars list)
           (assert (= (length vars) ,(length slots)))
           `(let* ((,',tmp ,,name)
                   (,',tmp1 (double-float-bits (realpart ,',tmp)))
                   (,',tmp2 (double-float-bits (imagpart ,',tmp))))
              (declare (type (unsigned-byte ,,real-width) ,',tmp1)
                       (type (unsigned-byte ,,imag-width) ,',tmp2))
              (let* ,(loop for var in vars
                           for rest on ',real-slots
                           for (slot-name slot-size _) = (car rest)
                           collect `(,var
                                     (prog1 (the (unsigned-byte ,slot-size)
                                                 (ldb (byte ,slot-size 0) ,',tmp1))
                                       ,@(when (cdr rest)
                                           `((setq ,',tmp1 (ash ,',tmp1 ,(- slot-size))))))))
                (let* ,(loop for var in (nthcdr ,(length real-slots) vars)
                             for rest on ',imag-slots
                             for (slot-name slot-size _) = (car rest)
                             collect `(,var
                                       (prog1 (the (unsigned-byte ,slot-size)
                                                   (ldb (byte ,slot-size 0) ,',tmp2))
                                         ,@(when (cdr rest)
                                             `((setq ,',tmp2 (ash ,',tmp2 ,(- slot-size))))))))
                  ,@body))))))))
