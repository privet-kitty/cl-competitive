(defpackage :cp/test/nearly-equal
  (:use :cl)
  (:import-from #:sb-kernel
                #:%array-dimension #:with-array-data #:%%data-vector-reffers%%
                #:%other-pointer-widetag #:truly-the)
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

(defun %array-nearly-equalp (threshold x y)
  (declare (array x y))
  (let ((rank (array-rank x)))
    (and
     (= rank (array-rank y))
     (dotimes (axis rank t)
       (unless (= (sb-kernel:%array-dimension x axis)
                  (sb-kernel:%array-dimension y axis))
         (return nil)))
     (with-array-data ((x x) (start-x) (end-x) :force-inline t
                                               :array-header-p t)
       (with-array-data ((y y) (start-y) (end-y) :force-inline t
                                                 :array-header-p t)
         (declare (ignore end-y))
         (let* ((reffers %%data-vector-reffers%%)
                (getter-x (truly-the function (svref reffers (sb-kernel::%other-pointer-widetag x))))
                (getter-y (truly-the function (svref reffers (%other-pointer-widetag y)))))
           (loop for x-i fixnum from start-x below end-x
                 for y-i fixnum from start-y
                 for x-el = (funcall getter-x x x-i)
                 for y-el = (funcall getter-y y y-i)
                 always (or (eq x-el y-el)
                            (nearly-equalp threshold x-el y-el)))))))))

(defun %vector-nearly-equalp (threshold x y)
  (declare (vector x y))
  (let ((length (length x)))
    (and (= length (length y))
         (with-array-data ((x x) (start-x) (end-x) :force-inline t
                                                   :check-fill-pointer t)
           (with-array-data ((y y) (start-y) (end-y) :force-inline t
                                                     :check-fill-pointer t)
             (declare (ignore end-y))
             (let* ((reffers %%data-vector-reffers%%)
                    (getter-x (truly-the function (svref reffers (%other-pointer-widetag x))))
                    (getter-y (truly-the function (svref reffers (%other-pointer-widetag y)))))
               (loop for x-i fixnum from start-x below end-x
                     for y-i fixnum from start-y
                     for x-el = (funcall getter-x x x-i)
                     for y-el = (funcall getter-y y y-i)
                     always (or (eq x-el y-el)
                                (nearly-equalp threshold x-el y-el)))))))))

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
         (and (vectorp y)
              (%vector-nearly-equalp threshold x y)))
        ((arrayp x)
         (and (arrayp y)
              (%array-nearly-equalp threshold x y)))
        (t nil)))
