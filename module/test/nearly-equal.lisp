(defpackage :cp/test/nearly-equal
  (:use :cl)
  (:export #:nearly= #:nearly<= #:nearly-equal #:nearly-equal-values #:nearly-equalp))
(in-package :cp/test/nearly-equal)

(defun nearly= (threshold &rest numbers)
  "THRESHOLD := acceptable absolute error."
  (loop for (value1 value2) on numbers
        while value2
        always (<= (abs (- value1 value2)) threshold)))

(define-compiler-macro nearly= (&whole form threshold &rest numbers)
  (cond ((<= (length numbers) 1) t)
        ((= (length numbers) 2)
         `(<= (abs (- ,(first numbers) ,(second numbers))) ,threshold))
        (t form)))

(defun nearly<= (threshold &rest numbers)
  "THRESHOLD := acceptable absolute error."
  (loop for (value1 value2) on numbers
        while value2
        always (<= (- value1 value2) threshold)))

(defun nearly-equal (threshold lst1 &rest lsts)
  "THRESHOLD := acceptable absolute error."
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

(defun nearly-equalp (threshold x y)
  "THRESHOLD := acceptable absolute error."
  (cond ((eq x y) t)
        ((numberp x) (and (numberp y) (nearly= threshold x y)))
        ((consp x)
         (and (consp y)
              (nearly-equalp threshold (car x) (car y))
              (nearly-equalp threshold (cdr x) (cdr y))))
        ((and (simple-vector-p x) (simple-vector-p y))
         (let ((len (length x)))
           (and (= len (length y))
                (loop for i below len
                      always (let ((a (svref x i)) (b (svref y i)))
                               (or (eq a b) (nearly-equalp threshold a b)))))))
        ((vectorp x)
         (let ((length (length x)))
           (and (vectorp y)
                (= length (length y))
                (dotimes (i length t)
                  (let ((x-el (aref x i))
                        (y-el (aref y i)))
                    (unless (or (eq x-el y-el)
                                (nearly-equalp threshold x-el y-el))
                      (return nil)))))))
        ((arrayp x)
         (and (arrayp y)
              (= (array-rank x) (array-rank y))
              (dotimes (axis (array-rank x) t)
                (unless (= (array-dimension x axis)
                           (array-dimension y axis))
                  (return nil)))
              (dotimes (index (array-total-size x) t)
                (let ((x-el (row-major-aref x index))
                      (y-el (row-major-aref y index)))
                  (unless (or (eq x-el y-el)
                              (nearly-equalp threshold x-el y-el))
                    (return nil))))))
        (t nil)))
