;; From alexandria
(defmacro ensure-gethash (key hash-table &optional default)
  (let ((%key (gensym))
        (%hash-table (gensym))
        (presentp (gensym))
        (value (gensym)))
    `(let ((,%key ,key)
           (,%hash-table ,hash-table))
       (multiple-value-bind (,value ,presentp) (gethash ,%key ,%hash-table)
         (if ,presentp
             (values ,value ,presentp)
             (values (setf (gethash ,%key ,%hash-table) ,default) nil))))))
