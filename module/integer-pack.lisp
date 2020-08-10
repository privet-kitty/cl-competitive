;; DEFINE-INTEGER-PACK and DEFINE-CONS-PACK are so to say poor man's variants of
;; DEFSTRUCT. Both "structures" can only have slots of fixed unsigned
;; bytes. DEFINE-INTEGER-PACK handles the concatenated slots as UNSIGNED-BYTE
;; and DEFINE-CONS-PACK handles them as (CONS (UNSIGNED-BYTE 62) (UNSIGNED-BYTE
;; 62)).

;; Example:
;; The following form defines the type NODE as (UNSIGNED-BYTE 9):
;; (define-integer-pack node (slot1 3) (slot2 5) (slot3 1))
;; This macro in addition defines relevant utilities: NODE-SLOT1, NODE-SLOT2,
;; NODE-SLOT3, setters and getters, PACK-NODE, the constructor, and
;; WITH-UNPACKING-NODE, the destructuring-bind-style macro.
;; 
;; DEFINE-CONS-PACK is almost the same as DEFINE-INTEGER-PACK though it will be
;; suitable for the total bits in the range [63, 124].

(defpackage :cp/integer-pack
  (:use :cl)
  (:export #:define-integer-pack #:define-cons-pack))
(in-package :cp/integer-pack)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun %concat-name (&rest args)
    (if (cdr args)
        (format nil "~A-~A"
                (car args)
                (apply #'%concat-name (cdr args)))
        (car args)))

  (defun %concat+name+ (&rest args)
    (format nil "+~A+" (apply #'%concat-name args))))

(defmacro define-integer-pack (name &rest slot-descriptions)
  (assert slot-descriptions () "~A has no slots." name)
  (let* ((packer-name (intern (%concat-name "PACK" name)))
         (unpacker-macro-name (intern (%concat-name "WITH-UNPACKING" name)))
         (total-size 0)
         (slots (loop with position = 0
                      for (slot-name slot-size) in slot-descriptions
                      collect (progn (check-type slot-name symbol)
                                     (check-type slot-size (integer 1))
                                     (list slot-name slot-size position))
                      do (incf position slot-size)
                      finally (setq total-size position)))
         (revslots (reverse slots))
         (new-value (gensym "NEW-VALUE"))
         (tmp (gensym)))
    `(progn
       (deftype ,name () '(unsigned-byte ,total-size))
       ;; define most positive integer for every slot as constant
       ,@(loop for (slot-name slot-size _) in slots
               collect `(defconstant ,(intern (%concat+name+ "MAX" name slot-name))
                          (- (ash 1 ,slot-size) 1)))
       ;; setter and getter
       ,@(loop for (slot-name slot-size slot-position) in slots
               for accessor-name = (intern (%concat-name name slot-name))
               append `((declaim (inline ,accessor-name
                                         (setf ,accessor-name)))
                        (defun ,accessor-name (,name)
                          (declare (type ,name ,name))
                          (ldb (byte ,slot-size ,slot-position) ,name))
                        (defun (setf ,accessor-name) (,new-value ,name)
                          (declare (type ,name ,name))
                          (setf (ldb (byte ,slot-size ,slot-position) ,name) ,new-value))))
       ;; constructor
       (declaim (inline ,packer-name))
       (defun ,packer-name ,(loop for slot in slots collect (car slot))
         (declare ,@(loop for (slot-name slot-size _) in slots
                          collect `(type (unsigned-byte ,slot-size) ,slot-name )))
         (let ((,tmp ,(caar revslots)))
           (declare (type (unsigned-byte ,total-size) ,tmp))
           ,@(loop for (slot-name slot-size _) in (cdr revslots)
                   collect `(setq ,tmp (logxor ,slot-name
                                               (the (unsigned-byte ,total-size)
                                                    (ash ,tmp ,slot-size)))))
           ,tmp))
       ;; destructuring-bind-style macro
       (defmacro ,unpacker-macro-name (vars ,name &body body)
         (check-type vars list)
         (assert (= (length vars) ,(length slots)))
         `(let ((,',tmp ,,name))
            (declare (type (unsigned-byte ,,total-size) ,',tmp))
            (let* ,(loop for var in vars
                         for rest on ',slots
                         for (slot-name slot-size _) = (car rest)
                         collect `(,var
                                   (prog1 (the (unsigned-byte ,slot-size)
                                               (ldb (byte ,slot-size 0) ,',tmp))
                                     ,@(when (cdr rest)
                                         `((setq ,',tmp (ash ,',tmp ,(- slot-size))))))))
              ,@body))))))

(defmacro define-cons-pack (name &rest slot-descriptions)
  (assert slot-descriptions () "~A has no slots." name)
  (labels ((extract-62bit-slots (list)
             (let ((position 0))
               (loop for (slot-name slot-size) in list
                     while (<= (+ position slot-size) 62)
                     collect (list slot-name slot-size position)
                     do (incf position slot-size)))))
    (let* ((packer-name (intern (%concat-name "PACK" name)))
           (unpacker-macro-name (intern (%concat-name "WITH-UNPACKING" name)))
           (new-value (gensym "NEW-VALUE"))
           (tmp1 (gensym))
           (tmp2 (gensym))
           (tmp (gensym))
           (car-slots (extract-62bit-slots slot-descriptions))
           (car-revslots (reverse car-slots))
           (cdr-slots (extract-62bit-slots (nthcdr (length car-slots) slot-descriptions)))
           (cdr-revslots (reverse cdr-slots))
           (slots (append car-slots cdr-slots)))
      (assert (= (+ (length car-slots) (length cdr-slots))
                 (length slot-descriptions))
              () "Size restriction violated: each cell <= 62 bit, total size <= 124 bit")
      (unless (> (length cdr-slots) 0)
        (error "Whole size is too small. Use DEFINE-INTEGER-PACK instead."))
      (let ((car-width (+ (second (first car-revslots))
                          (third (first car-revslots))))
            (cdr-width (+ (second (first cdr-revslots))
                          (third (first cdr-revslots)))))
        `(progn
           (deftype ,name () '(cons (unsigned-byte ,car-width) (unsigned-byte ,cdr-width)))
           ;; define most positive integer for every slot as constant
           ,@(loop for (slot-name slot-size _) in slots
                   collect `(defconstant ,(intern (%concat+name+ "MAX" name slot-name))
                              (- (ash 1 ,slot-size) 1)))
           ;; setter and getter
           ,@(loop for slot in car-slots
                   for (slot-name slot-size slot-position) = slot
                   for accessor-name = (intern (%concat-name name slot-name))
                   append `((declaim (inline ,accessor-name (setf ,accessor-name)))
                            (defun ,accessor-name (,name)
                              (declare (type ,name ,name))
                              (ldb (byte ,slot-size ,slot-position)
                                   (the (unsigned-byte ,car-width) (car ,name))))
                            (defun (setf ,accessor-name) (,new-value ,name)
                              (declare (type ,name ,name))
                              (setf (ldb (byte ,slot-size ,slot-position)
                                         (the (unsigned-byte ,car-width) (car ,name)))
                                    ,new-value))))
           ,@(loop for slot in cdr-slots
                   for (slot-name slot-size slot-position) = slot
                   for accessor-name = (intern (%concat-name name slot-name))
                   append `((declaim (inline ,accessor-name (setf ,accessor-name)))
                            (defun ,accessor-name (,name)
                              (declare (type ,name ,name))
                              (ldb (byte ,slot-size ,slot-position)
                                   (the (unsigned-byte ,cdr-width) (cdr ,name))))
                            (defun (setf ,accessor-name) (,new-value ,name)
                              (declare (type ,name ,name))
                              (setf (ldb (byte ,slot-size ,slot-position)
                                         (the (unsigned-byte ,cdr-width) (cdr ,name)))
                                    ,new-value))))
           ;; constructor
           (declaim (inline ,packer-name))
           (defun ,packer-name ,(loop for slot in slots collect (car slot))
             (declare ,@(loop for (slot-name slot-size slot-position) in slots
                              collect `(type (unsigned-byte ,slot-size) ,slot-name)))
             (let ((,tmp1 ,(caar car-revslots))
                   (,tmp2 ,(caar cdr-revslots)))
               (declare (type (unsigned-byte ,car-width) ,tmp1)
                        (type (unsigned-byte ,cdr-width) ,tmp2))
               ,@(loop for (slot-name slot-size _) in (rest car-revslots)
                       collect `(setq ,tmp1 (logxor ,slot-name
                                                    (the (unsigned-byte ,car-width)
                                                         (ash ,tmp1 ,slot-size)))))
               ,@(loop for (slot-name slot-size _) in (rest cdr-revslots)
                       collect `(setq ,tmp2 (logxor ,slot-name
                                                    (the (unsigned-byte ,cdr-width)
                                                         (ash ,tmp2 ,slot-size)))))
               (cons ,tmp1 ,tmp2)))
           ;; destructuring-bind-style macro
           (defmacro ,unpacker-macro-name (vars ,name &body body)
             (check-type vars list)
             (assert (= (length vars) ,(length slots)))
             `(let* ((,',tmp ,,name)
                     (,',tmp1 (car ,',tmp))
                     (,',tmp2 (cdr ,',tmp)))
                (declare (type (unsigned-byte ,,car-width) ,',tmp1)
                         (type (unsigned-byte ,,cdr-width) ,',tmp2))
                (let* ,(loop for var in vars
                             for rest on ',car-slots
                             for (slot-name slot-size _) = (car rest)
                             collect `(,var
                                       (prog1 (the (unsigned-byte ,slot-size)
                                                   (ldb (byte ,slot-size 0) ,',tmp1))
                                         ,@(when (cdr rest)
                                             `((setq ,',tmp1 (ash ,',tmp1 ,(- slot-size))))))))
                  (let* ,(loop for var in (nthcdr ,(length car-slots) vars)
                               for rest on ',cdr-slots
                               for (slot-name slot-size _) = (car rest)
                               collect `(,var
                                         (prog1 (the (unsigned-byte ,slot-size)
                                                     (ldb (byte ,slot-size 0) ,',tmp2))
                                           ,@(when (cdr rest)
                                               `((setq ,',tmp2 (ash ,',tmp2 ,(- slot-size))))))))
                    ,@body)))))))))
