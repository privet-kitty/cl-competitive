;;
;; Matrix multiplication over semiring
;;

(defun gemm! (a b c &key (op+ #'+) (op* #'*) (identity+ 0))
  "Calculates C := A*B. Destructively modifies C. (OP+, OP*) must form a
semiring. IDENTITY+ is the identity element w.r.t. OP+."
  (declare ((simple-array * (* *)) a b c))
  (dotimes (row (array-dimension a 0))
    (dotimes (col (array-dimension b 1))
      (let ((res identity+))
        (dotimes (k (array-dimension a 1))
          (setf res
                (funcall op+ (funcall op* (aref a row k) (aref b k col)))))
        (setf (aref c row col) res))))
  c)


(declaim (inline gemm))
(defun gemm (a b &key (op+ #'+) (op* #'*) (identity+ 0))
  "Calculates A*B. (OP+, OP*) must form a semiring. IDENTITY+ is the identity
element w.r.t. OP+."
  (declare ((simple-array * (* *)) a b)
           (function op+ op*))
  (let ((c (make-array (list (array-dimension a 0) (array-dimension b 1))
                       :element-type (array-element-type a))))
    (dotimes (row (array-dimension a 0))
      (dotimes (col (array-dimension b 1))
        (let ((res identity+))
          (dotimes (k (array-dimension a 1))
            (setf res
                  (funcall op+ res (funcall op* (aref a row k) (aref b k col)))))
          (setf (aref c row col) res))))
    c))

(declaim (inline gemv))
(defun gemv (a x &key (op+ #'+) (op* #'*) (identity+ 0))
  "Calculates A*x for a matrix A and a vector x. (OP+, OP*) must form a
semiring. IDENTITY+ is the identity element w.r.t. OP+."
  (declare ((simple-array * (* *)) a)
           ((simple-array * (*)) x)
           (function op+ op*))
  (let ((y (make-array (array-dimension a 0) :element-type (array-element-type x))))
    (dotimes (i (length y))
      (let ((res identity+))
        (dotimes (j (length x))
          (setf res
                (funcall op+ res (funcall op* (aref a i j) (aref x j)))))
        (setf (aref y i) res)))
    y))
