;;;
;;; Memoization macro
;;;

;;
;; Basic usage:
;; (with-cache (:hash-table :test #'equal :key #'cons)
;;   (defun add (a b)
;;     (+ a b)))
;; This function caches the returned values for already passed combinations of
;; arguments. In this case ADD stores the key (CONS A B) and the corresponding
;; value to a hash-table when evaluating (ADD A B) for the first time; ADD
;; returns the stored value when it is called with the same arguments
;; (w.r.t. EQUAL) again.
;;
;; The storage for the cache can be hash-table or array. Let's see an example
;; for array:
;; (with-cache (:array (10 20 30) :initial-element -1 :element-type 'fixnum)
;;   (defun foo (a b c) ... ))
;; This form caches the value of FOO in the array created by (make-array (list
;; 10 20 30) :initial-element -1 :element-type 'fixnum). Note that
;; INITIAL-ELEMENT must always be given here as it is used as the flag for `not
;; yet stored'. (Therefore INITIAL-ELEMENT should be a value FOO doesn't take.)
;;
;; If you want to ignore some arguments, you can use `*' in dimensions:
;; (with-cache (:array (10 10 * 10) :initial-element -1)
;;   (defun foo (a b c d) ...)) ; => C is ignored when querying or storing cache
;;
;; Available definition forms in WITH-CACHE are DEFUN, LABELS, FLET, and
;; SB-INT:NAMED-LET.
;;
;; You can debug the memoized function by :DEBUG option:
;; (with-cache (:array (10 10) :initial-element -1 :debug t)
;;   (defun foo (x y) ...))
;; Then FOO is traced as with CL:TRACE.
;;

;; FIXME: *RECURSION-DEPTH* should be included within the macro.
(declaim (type (integer 0 #.most-positive-fixnum) *recursion-depth*))
(defparameter *recursion-depth* 0)

(defmacro with-cache ((cache-type &rest cache-attribs) def-form)
  "CACHE-TYPE := :HASH-TABLE | :ARRAY"
  (assert (member cache-type '(:hash-table :array)))
  (let* ((dimensions-with-* (when (eql cache-type :array) (first cache-attribs)))
         (dimensions (remove '* dimensions-with-*))
         (rank (length dimensions))
         (rest-attribs (ecase cache-type
                         (:hash-table cache-attribs)
                         (:array (cdr cache-attribs))))
         (key (prog1 (getf rest-attribs :key) (remf rest-attribs :key)))
         (debug (prog1 (getf rest-attribs :debug) (remf rest-attribs :debug)))
         (cache-form (case cache-type
                       (:hash-table `(make-hash-table ,@rest-attribs))
                       (:array `(make-array (list ,@dimensions) ,@rest-attribs))))
         (initial-element (when (eql cache-type :array)
                            (assert (member :initial-element rest-attribs))
                            (getf rest-attribs :initial-element))))
    (let ((cache (gensym "CACHE"))
          (value (gensym))
	  (present-p (gensym))
          (name-alias (gensym))
	  (args-lst (gensym))
          (indices (loop repeat rank collect (gensym))))
      (labels ((debug (name args obj)
                 (let ((value (gensym)))
                   (if debug
                       `(progn
                          (format t "~&~A~A: (~A ~{~A~^ ~}) =>"
                                  (make-string *recursion-depth*
                                               :element-type 'base-char
                                               :initial-element #\ )
                                  *recursion-depth*
                                  ',name
                                  (list ,@args))
                          (let ((,value (let ((*recursion-depth* (1+ *recursion-depth*)))
                                          ,obj)))
                            (format t "~&~A~A: (~A ~{~A~^ ~}) => ~A"
                                    (make-string *recursion-depth*
                                                 :element-type 'base-char
                                                 :initial-element #\ )
                                    *recursion-depth*
                                    ',name
                                    (list ,@args)
                                    ,value)
                            ,value))
                       obj)))
               (make-cache-check-form (cache-type name args)
                 (debug name
                        args
                        (case cache-type
                          (:hash-table
                           `(let ((,args-lst (funcall ,(or key #'list) ,@args)))
                              (multiple-value-bind (,value ,present-p)
                                  (gethash ,args-lst ,cache)
                                (if ,present-p
                                    ,value
                                    (setf (gethash ,args-lst ,cache)
                                          (,name-alias ,@args))))))
                          (:array
                           (let ((memoized-args (loop for dimension in dimensions-with-*
                                                      for arg in args
                                                      unless (eql dimension '*)
                                                      collect arg)))
                             (if key
                                 `(multiple-value-bind ,indices
                                      (funcall ,key ,@memoized-args)
                                    (let ((,value (aref ,cache ,@indices)))
                                      (if (eql ,initial-element ,value)
                                          (setf (aref ,cache ,@indices)
                                                (,name-alias ,@args))
                                          ,value)))
                                 `(let ((,value (aref ,cache ,@memoized-args)))
                                    (if (eql ,initial-element ,value)
                                        (setf (aref ,cache ,@memoized-args)
                                              (,name-alias ,@args))
                                        ,value))))))))
               (make-reset-form (cache-type)
                 (case cache-type
                   (:hash-table `(setf ,cache (make-hash-table ,@rest-attribs)))
                   (:array `(prog1 nil
                              (fill (array-storage-vector ,cache) ,initial-element)))))
               (make-reset-name (name)
                 (intern (format nil "RESET-~A" (symbol-name name))))
               (extract-declarations (body)
                 (remove-if-not (lambda (form) (eql 'declare (car form))) body)))
        (ecase (car def-form)
          ((defun)
           (destructuring-bind (_ name args &body body) def-form
             (declare (ignore _))
             `(let ((,cache ,cache-form))
                (defun ,(make-reset-name name) () ,(make-reset-form cache-type))
                (defun ,name ,args
                  ,@(extract-declarations body)
                  (labels ((,name-alias ,args ,@body))
                    (declare (inline ,name-alias))
                    ,(make-cache-check-form cache-type name args))))))
          ((nlet #+sbcl sb-int:named-let)
           (destructuring-bind (_ name bindings &body body) def-form
             (declare (ignore _))
             `(let ((,cache ,cache-form))
                (,(car def-form) ,name ,bindings
                 ,@(extract-declarations body)
                 ,(let ((args (mapcar (lambda (x) (if (atom x) x (car x))) bindings)))
                    `(labels ((,name-alias ,args ,@body))
                       (declare (inline ,name-alias))
                       ,(make-cache-check-form cache-type name args)))))))
          ((labels flet)
           (destructuring-bind (_ definitions &body labels-body) def-form
             (declare (ignore _))
             (destructuring-bind (name args &body body) (car definitions)
               `(let ((,cache ,cache-form))
                  (,(car def-form)
                   ((,(make-reset-name name) () ,(make-reset-form cache-type))
                    (,name ,args
                           ,@(extract-declarations body)
                           (labels ((,name-alias ,args ,@body))
                             (declare (inline ,name-alias))
                             ,(make-cache-check-form cache-type name args)))
                    ,@(cdr definitions))
                   (declare (ignorable #',(make-reset-name name)))
                   ,@labels-body))))))))))
