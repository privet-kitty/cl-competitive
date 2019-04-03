;; (with-memoizing (:hash-table :test #'equal :key #'cons)
;;   (defun ...))
;; (with-memoizing (:array (10 10 * 10) :initial-element -1 :element-type 'fixnum)
;;   (defun foo (a b c d) ...) ; C is ignored.
(declaim (type (integer 0 #.most-positive-fixnum) *recursion-depth*))
(defparameter *recursion-depth* 0)

(defmacro with-memoizing (cache-attribs def-form)
  (let* ((cache-attribs (if (atom cache-attribs) (list cache-attribs) cache-attribs))
         (cache-type (first cache-attribs))
         (dimensions-with-* (when (eql cache-type :array) (second cache-attribs)))
         (dimensions (remove '* dimensions-with-*))
         (rank (length dimensions))
         (rest-attribs (ecase cache-type
                         (:hash-table (cdr cache-attribs))
                         (:array (cddr cache-attribs))))
         (key (prog1 (getf rest-attribs :key) (remf rest-attribs :key)))
         (debug (prog1 (getf rest-attribs :debug) (remf rest-attribs :debug)))
         (cache-form (case cache-type
                       (:hash-table `(make-hash-table ,@rest-attribs))
                       (:array `(make-array (list ,@dimensions) ,@rest-attribs))))
         (initial-element (when (eql cache-type :array)
                            (assert (member :initial-element rest-attribs))
                            (getf rest-attribs :initial-element))))
    (let ((cache (gensym))
          (value (gensym))
	  (present-p (gensym))
          (name-alias (gensym))
	  (args-lst (gensym))
          (indices (loop repeat rank collect (gensym))))
      (labels ((debug (name args obj)
                 (let ((value (gensym)))
                   (if debug
                       `(progn
                          (format t "~A~A: (~A ~{~A~^ ~}) =>~%"
                                  (make-string *recursion-depth*
                                               :element-type 'base-char
                                               :initial-element #\ )
                                  *recursion-depth*
                                  ',name
                                  (list ,@args))
                          (let ((,value (let ((*recursion-depth* (1+ *recursion-depth*)))
                                          ,obj)))
                            (format t "~A~A: (~A ~{~A~^ ~}) => ~A~%"
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
          ((nlet)
           (destructuring-bind (_ name bindings &body body) def-form
             (declare (ignore _))
             `(let ((,cache ,cache-form))
                (nlet ,name ,bindings
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

;; (test with-memoizing
;;   (finishes (macroexpand `(with-memoizing (:hash-table :test #'equal)
;;                             (defun add (x y) (+ x y)))))
;;   (finishes (macroexpand `(with-memoizing (:array '(10 10)
;;                                            :element-type 'fixnum
;;                                            :initial-element -1)
;;                             (defun add (x y) (+ x y)))))
;;   (finishes (macroexpand `(with-memoizing (:array '(10 10)
;;                                            :element-type 'fixnum
;;                                            :initial-element -1)
;;                             (labels ((add (x y) (+ x y))
;; 		                     (my-print (x) (print x)))
;; 	                      (add 1 2))))))
