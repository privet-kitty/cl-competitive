;;;
;;; Modular arithmetic
;;;

;; Blankinship algorithm
;; Reference: https://topcoder-g-hatena-ne-jp.jag-icpc.org/spaghetti_source/20130126/ (Japanese)
(declaim (ftype (function * (values fixnum fixnum &optional)) %ext-gcd))
(defun %ext-gcd (a b)
  (declare (optimize (speed 3) (safety 0))
           (fixnum a b))
  (let ((y 1)
        (x 0)
        (u 1)
        (v 0))
    (declare (fixnum y x u v))
    (loop (when (zerop a)
            (return (values x y)))
          (let ((q (floor b a)))
            (decf x (the fixnum (* q u)))
            (rotatef x u)
            (decf y (the fixnum (* q v)))
            (rotatef y v)
            (decf b (the fixnum (* q a)))
            (rotatef b a)))))

;; Simple recursive version. A bit slower but more comprehensible.
;; https://cp-algorithms.com/algebra/extended-euclid-algorithm.html (English)
;; https://drken1215.hatenablog.com/entry/2018/06/08/210000 (Japanese)
;; (defun %ext-gcd (a b)
;;   (declare (optimize (speed 3) (safety 0))
;;            (fixnum a b))
;;   (if (zerop b)
;;       (values 1 0)
;;       (multiple-value-bind (p q) (floor a b) ; a = pb + q
;;         (multiple-value-bind (v u) (%ext-gcd b q)
;;           (declare (fixnum u v))
;;           (values u (the fixnum (- v (the fixnum (* p u)))))))))

;; TODO: deal with bignums
(declaim (inline ext-gcd))
(defun ext-gcd (a b)
  "Returns two integers X and Y which satisfy AX + BY = gcd(A, B)."
  (declare ((integer #.(- most-positive-fixnum) #.most-positive-fixnum) a b))
  (if (>= a 0)
      (if (>= b 0)
          (%ext-gcd a b)
          (multiple-value-bind (x y) (%ext-gcd a (- b))
            (declare (fixnum x y))
            (values x (- y))))
      (if (>= b 0)
          (multiple-value-bind (x y) (%ext-gcd (- a) b)
            (declare (fixnum x y))
            (values (- x) y))
          (multiple-value-bind (x y) (%ext-gcd (- a) (- b))
            (declare (fixnum x y))
            (values (- x) (- y))))))

(declaim (inline mod-inverse)
         (ftype (function * (values (mod #.most-positive-fixnum) &optional)) mod-inverse))

;; (defun mod-inverse (a modulus)
;;   "Solves ax ≡ 1 mod m. A and M must be coprime."
;;   (declare (integer a)
;;            ((integer 1 #.most-positive-fixnum) modulus))
;;   (mod (%ext-gcd (mod a modulus) modulus) modulus))

;; FIXME: Perhaps no advantage in efficiency? Then I should use the above simple
;; code.
(defun mod-inverse (a modulus)
  "Solves ax ≡ 1 mod m. A and M must be coprime."
  (declare ((integer 1 #.most-positive-fixnum) modulus))
  (let ((a (mod a modulus))
        (b modulus)
        (u 1)
        (v 0))
    (declare (fixnum a b u v))
    (loop until (zerop b)
          for quot = (floor a b)
          do (decf a (the fixnum (* quot b)))
             (rotatef a b)
             (decf u (the fixnum (* quot v)))
             (rotatef u v))
    (setq u (mod u modulus))
    (if (< u 0)
        (+ u modulus)
        u)))

;; not tested
;; TODO: move to another file
(declaim (inline mod-binomial))
(defun mod-binomial (n k modulus)
  (declare ((integer 0 #.most-positive-fixnum) modulus))
  (if (or (< n k) (< n 0) (< k 0))
      0
      (let ((k (if (< k (- n k)) k (- n k)))
            (num 1)
            (denom 1))
        (declare ((integer 0) k num denom))
        (loop for x from n above (- n k)
              do (setq num (mod (* num x) modulus)))
        (loop for x from 1 to k
              do (setq denom (mod (* denom x) modulus)))
        (mod (* num (mod-inverse denom modulus)) modulus))))

(declaim (ftype (function * (values (or null (integer 1 #.most-positive-fixnum)) &optional)) mod-log))

(defun mod-log (x y modulus)
  "Returns the smallest positive integer k that satiefies x^k ≡ y mod p.
Returns NIL if it is infeasible."
  (declare (optimize (speed 3))
           (integer x y)
           ((integer 1 #.most-positive-fixnum) modulus))
  (let ((x (mod x modulus))
        (y (mod y modulus))
        (g (gcd x modulus)))
    (declare (optimize (safety 0))
             ((mod #.most-positive-fixnum) x y g))
    (if (= g 1)
        ;; coprime case
        (let* ((m (+ 1 (isqrt (- modulus 1)))) ; smallest integer equal to or
                                               ; larger than sqrt(p)
               (x^m (loop for i below m
                          for res of-type (integer 0 #.most-positive-fixnum) = x
                          then (mod (* res x) modulus)
                          finally (return res)))
               (table (make-hash-table :size m :test 'eq)))
          ;; Constructs TABLE: yx^j |-> j (j = 0, ..., m-1)
          (loop for j from 0 below m
                for res of-type (integer 0 #.most-positive-fixnum) = y
                then (mod (* res x) modulus)
                do (setf (gethash res table) j))
          ;; Finds i and j that satisfy (x^m)^i = yx^j and returns m*i-j
          (loop for i from 1 to m
                for x^m^i of-type (integer 0 #.most-positive-fixnum) = x^m
                then (mod (* x^m^i x^m) modulus)
                for j = (gethash x^m^i table)
                when j
                do (locally
                       (declare ((integer 0 #.most-positive-fixnum) j))
                     (return (- (* i m) j)))
                finally (return nil)))
        ;; If x and p are not coprime, let g := gcd(x, p), x := gx', y := gy', p
        ;; := gp' and solve x^(k-1) ≡ y'x'^(-1) mod p' instead. See
        ;; https://math.stackexchange.com/questions/131127/ for the detail.
        (if (= x y)
            ;; This is tha special treatment for the case x ≡ y. Without this
            ;; (mod-log 4 0 4) returns not 1 but 2.
            1
            (multiple-value-bind (y-prime rem) (floor y g)
              (if (zerop rem)
                  (let* ((x-prime (floor x g))
                         (p-prime (floor modulus g))
                         (next-rhs (mod (* y-prime (mod-inverse x-prime p-prime)) p-prime))
                         (res (mod-log x next-rhs p-prime)))
                    (declare ((integer 0 #.most-positive-fixnum) x-prime p-prime next-rhs))
                    (if res (+ 1 res) nil))
                  nil))))))

(declaim (inline %calc-min-factor))
(defun %calc-min-factor (x alpha)
  "Returns k, so that x+k*alpha is the smallest non-negative number."
  (if (plusp alpha)
      (ceiling (- x) alpha)
      (floor (- x) alpha)))

(declaim (inline %calc-max-factor))
(defun %calc-max-factor (x alpha)
  "Returns k, so that x+k*alpha is the largest non-positive number."
  (if (plusp alpha)
      (floor (- x) alpha)
      (ceiling (- x) alpha)))

(defun solve-bezout (a b c &optional min max)
  "Returns an integer solution of a*x+b*y = c if it exists, otherwise
returns (VALUES NIL NIL).

If MIN is specified and MAX is null, the returned x is the smallest integer
equal to or larger than MIN. If MAX is specified and MIN is null, x is the
largest integer equal to or smaller than MAX. If the both are specified, x is an
integer in [MIN, MAX]. This function returns NIL when no x that satisfies the
given condition exists."
  (declare (fixnum a b c)
           ((or null fixnum) min max))
  (let ((gcd-ab (gcd a b)))
    (if (zerop (mod c gcd-ab))
        (multiple-value-bind (init-x init-y) (ext-gcd a b)
          (let* ((factor (floor c gcd-ab))
                 ;; m*x0 + n*y0 = d
                 (x0 (* init-x factor))
                 (y0 (* init-y factor)))
            (if (and (null min) (null max))
                (values x0 y0)
                (let (;; general solution: x = x0 + kΔx, y = y0 - kΔy
                      (deltax (floor b gcd-ab))
                      (deltay (floor a gcd-ab)))
                  (if min
                      (let* ((k-min (%calc-min-factor (- x0 min) deltax))
                             (x (+ x0 (* k-min deltax)))
                             (y (- y0 (* k-min deltay))))
                        (if (and max (> x max))
                            (values nil nil)
                            (values x y)))
                      (let* ((k-max (%calc-max-factor (- x0 max) deltax))
                             (x (+ x0 (* k-max deltax)))
                             (y (- y0 (* k-max deltay))))
                        (if (<= x max)
                            (values x y)
                            (values nil nil))))))))
        (values nil nil))))

;; Reference: http://drken1215.hatenablog.com/entry/2019/03/20/202800 (Japanese)
(declaim (inline mod-echelon!))
(defun mod-echelon! (matrix modulus &optional extended)
  "Returns the row echelon form of MATRIX by gaussian elimination and returns
the rank as the second value.

This function destructively modifies MATRIX."
  (declare ((integer 1 #.most-positive-fixnum) modulus))
  (destructuring-bind (m n) (array-dimensions matrix)
    (declare ((integer 0 #.most-positive-fixnum) m n))
    (dotimes (i m)
      (dotimes (j n)
        (setf (aref matrix i j) (mod (aref matrix i j) modulus))))
    (let ((rank 0))
      (dotimes (target-col (if extended (- n 1) n))
        (let ((pivot-row (do ((i rank (+ 1 i)))
                             ((= i m) -1)
                           (unless (zerop (aref matrix i target-col))
                             (return i)))))
          (when (>= pivot-row 0)
            ;; swap rows
            (loop for j from target-col below n
                  do (rotatef (aref matrix rank j) (aref matrix pivot-row j)))
            (let ((inv (mod-inverse (aref matrix rank target-col) modulus)))
              (dotimes (j n)
                (setf (aref matrix rank j)
                      (mod  (* inv (aref matrix rank j)) modulus)))
              (dotimes (i m)
                (unless (or (= i rank) (zerop (aref matrix i target-col)))
                  (let ((factor (aref matrix i target-col)))
                    (loop for j from target-col below n
                          do (setf (aref matrix i j)
                                   (mod (- (aref matrix i j)
                                           (mod (* (aref matrix rank j) factor) modulus))
                                        modulus)))))))
            (incf rank))))
      (values matrix rank))))

(declaim (inline mod-inverse-matrix!))
(defun mod-inverse-matrix! (matrix modulus)
  "Returns the inverse of MATRIX by gaussian elimination if it exists and
returns NIL otherwise. This function destructively modifies MATRIX."
  (declare ((integer 1 #.most-positive-fixnum) modulus))
  (destructuring-bind (m n) (array-dimensions matrix)
    (declare ((integer 0 #.most-positive-fixnum) m n))
    (assert (= m n))
    (dotimes (i n)
      (dotimes (j n)
        (setf (aref matrix i j) (mod (aref matrix i j) modulus))))
    (let ((result (make-array (list n n) :element-type (array-element-type matrix))))
      (dotimes (i n) (setf (aref result i i) 1))
      (dotimes (target n)
        (let ((pivot-row (do ((i target (+ 1 i)))
                             ((= i n) -1)
                           (unless (zerop (aref matrix i target))
                             (return i)))))
          (when (= pivot-row -1) ; when singular
            (return-from mod-inverse-matrix! nil))
          (loop for j from target below n
                do (rotatef (aref matrix target j) (aref matrix pivot-row j))
                   (rotatef (aref result target j) (aref result pivot-row j)))
          (let ((inv (mod-inverse (aref matrix target target) modulus)))
            ;; process the pivot row
            (dotimes (j n)
              (setf (aref matrix target j)
                    (mod  (* inv (aref matrix target j)) modulus))
              (setf (aref result target j)
                    (mod  (* inv (aref result target j)) modulus)))
            ;; eliminate the column
            (dotimes (i n)
              (unless (or (= i target) (zerop (aref matrix i target)))
                (let ((factor (aref matrix i target)))
                  (dotimes (j n)
                    (setf (aref matrix i j)
                          (mod (- (aref matrix i j)
                                  (mod (* (aref matrix target j) factor) modulus))
                               modulus))
                    (setf (aref result i j)
                          (mod (- (aref result i j)
                                  (mod (* (aref result target j) factor) modulus))
                               modulus)))))))))
      result)))

(declaim (inline mod-solve-linear-system))
(defun mod-solve-linear-system (matrix vector modulus)
  "Solves Ax ≡ b and returns a root vector if it exists. Otherwise it returns
NIL. In addition, this function returns the rank of A as the second value."
  (destructuring-bind (m n) (array-dimensions matrix)
    (declare ((integer 0 #.most-positive-fixnum) m n))
    (assert (= n (length vector)))
    (let ((extended (make-array (list m (+ n 1)) :element-type (array-element-type matrix))))
      (dotimes (i m)
        (dotimes (j n) (setf (aref extended i j) (aref matrix i j)))
        (setf (aref extended i n) (aref vector i)))
      (let ((rank (nth-value 1 (mod-echelon! extended modulus t))))
        (if (loop for i from rank below m
                  always (zerop (aref extended i n)))
            (let ((result (make-array m
                                      :element-type (array-element-type matrix)
                                      :initial-element 0)))
              (dotimes (i rank)
                (setf (aref result i) (aref extended i n)))
              (values result rank))
            (values nil rank))))))
