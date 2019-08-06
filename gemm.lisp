;;
;; Matrix multiplication over semiring
;;

;; NOTE: These funcions are slow on SBCL version earlier than 1.5.6 as the type
;; propagation of MAKE-ARRAY doesn't work. The following files are required to
;; enable the optimization.
;; version < 1.5.0: array-element-type.lisp, make-array-header.lisp
;; version < 1.5.6: make-array-header.lisp
(defun gemm! (a b c &key (op+ #'+) (op* #'*) (identity+ 0))
  "Calculates C := A*B. This function destructively modifies C. (OP+, OP*) must
comprise a semiring. IDENTITY+ is the identity element w.r.t. OP+."
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
  "Calculates A*B. (OP+, OP*) must comprise a semiring. IDENTITY+ is the
identity element w.r.t. OP+."
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

(declaim (inline matrix-power))
(defun matrix-power (base power &key (op+ #'+) (op* #'op*) (identity+ 0) (identity* 1))
  (declare ((simple-array * (* *)) base)
           (function op+ op*)
           ((integer 0 #.most-positive-fixnum) power))
  (let ((size (array-dimension base 0)))
    (assert (= size (array-dimension base 1)))
    (let ((iden (make-array (array-dimensions base)
                            :element-type (array-element-type base)
                            :initial-element identity+)))
      (dotimes (i size)
        (setf (aref iden i i) identity*))
      (labels ((recur (p)
                 (declare ((integer 0 #.most-positive-fixnum) p))
                 (cond ((zerop p) iden)
                       ((evenp p)
                        (let ((res (recur (ash p -1))))
                          (gemm res res :op+ op+ :op* op* :identity+ identity+)))
                       (t
                        (gemm base (recur (- p 1))
                              :op+ op+ :op* op* :identity+ identity+)))))
        (recur power)))))

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
