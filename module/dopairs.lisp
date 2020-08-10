;; NOTE: not enclosed with (BLOCK NIL)
(defmacro dopairs ((var1 var2 list &optional result) &body body)
  "Iterates BODY for each subset of LIST containing two elements."
  (let ((suffix (gensym))
        (_list (gensym)))
    `(let ((,_list ,list))
       (loop for ,suffix on ,_list
             for ,var1 = (car ,suffix)
             do (dolist (,var2 (cdr ,suffix))
                  ,@body))
       ,result)))
