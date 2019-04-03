(defun nearly= (threshold &rest numbers)
  "THRESHOLD is acceptable absolute error."
  (loop for (value1 value2) on numbers
        while value2
        always (<= (abs (- value1 value2)) threshold)))

(define-compiler-macro nearly= (&whole form threshold &rest numbers)
  (cond ((<= (length numbers) 1) t)
        ((= (length numbers) 2)
         `(<= (abs (- ,(first numbers) ,(second numbers))) ,threshold))
        (t form)))

(defun nearly<= (threshold &rest numbers)
  "THRESHOLD is acceptable absolute error."
  (loop for (value1 value2) on numbers
        while value2
        always (<= (- value1 value2) threshold)))

(defun nearly-equal (threshold lst1 &rest lsts)
  "THRESHOLD is acceptable absolute error."
  (if (null lst1)
      t
      (and (apply #'nearly= threshold
                  (car lst1)
                  (mapcar #'car lsts))
           (apply #'nearly-equal threshold
                  (cdr lst1)
                  (mapcar #'cdr lsts)))))

(defmacro nearly-equal-values (threshold &rest forms)
  `(nearly-equal ,threshold
                 ,@(mapcar #'(lambda (x) `(multiple-value-list ,x))
                           forms)))
