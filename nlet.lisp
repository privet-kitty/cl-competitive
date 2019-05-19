;; Scheme-style named let
(defmacro nlet (name args &body body)
  (labels ((ensure-list (x) (if (listp x) x (list x))))
    (let ((args (mapcar #'ensure-list args)))
      `(labels ((,name ,(mapcar #'car args) ,@body))
         (,name ,@(mapcar #'cadr args))))))
