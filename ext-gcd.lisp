;; TODO: deal with bignums
(declaim (inline ext-gcd))
(defun ext-gcd (a b)
  "Returns two integers X and Y where AX + BY = gcd(A, B) holds."
  (declare (fixnum a b))
  (labels ((%gcd (a b)
             (declare (fixnum a b))
             (if (zerop b)
                 (values 1 0)
                 (multiple-value-bind (p q) (floor a b) ; a = pb + q
                   (multiple-value-bind (v u) (%gcd b q)
                     (declare (fixnum u v))
                     (values u (the fixnum (- v (the fixnum (* p u))))))))))
    (if (>= a 0)
        (if (>= b 0)
            (%gcd a b)
            (multiple-value-bind (x y) (%gcd a (- b))
              (declare (fixnum x y))
              (values x (- y))))
        (if (>= b 0)
            (multiple-value-bind (x y) (%gcd (- a) b)
              (declare (fixnum x y))
              (values (- x) y))
            (multiple-value-bind (x y) (%gcd (- a) (- b))
              (declare (fixnum x y))
              (values (- x) (- y)))))))

(declaim (inline mod-inverse))
(defun mod-inverse (a m)
  "Solves ax ≡ 1 mod m. A and M must be coprime."
  (declare (integer a)
           ((integer 1 #.most-positive-fixnum) m))
  (mod (ext-gcd (mod a m) m) m))

(declaim (ftype (function * (values (or null (integer 1 #.most-positive-fixnum)) &optional)) mod-log))
(defun mod-log (x y divisor)
  "Returns the smallest positive integer k that satiefies x^k ≡ y mod p.
Returns NIL if it is infeasible."
  (declare (integer x y)
           ((integer 1 #.most-positive-fixnum) divisor))
  (let ((x (mod x divisor))
        (y (mod y divisor)))
    (declare ((mod #.most-positive-fixnum) x y))
    (if (= 1 (gcd x divisor))
        (let* ((m (+ 1 (isqrt (- divisor 1)))) ; smallest integer equal or larger than √p
               (x^m (loop for i below m
                          for res of-type (integer 0 #.most-positive-fixnum) = x
                          then (mod (* res x) divisor)
                          finally (return res)))
               (table (make-hash-table :size m)))
          ;; Constructs TABLE: yx^j |-> j (j = 0, ..., m-1)
          (loop for j from 0 below m
                for res of-type (integer 0 #.most-positive-fixnum) = y
                then (mod (* res x) divisor)
                do (setf (gethash res table) j))
          ;; Finds i, j that satisfies (x^m)^i = yx^j and returns m*i-j
          (loop for i from 1 to m
                for x^m^i of-type (integer 0 #.most-positive-fixnum) = x^m
                then (mod (* x^m^i x^m) divisor)
                for j = (gethash x^m^i table)
                when j
                do (locally
                       (declare ((integer 0 #.most-positive-fixnum) j))
                     (return (- (* i m) j)))
                finally (return nil)))
        ;; If x and p are not coprime, let g := gcd(x, p), x := gx', y := gy', p
        ;; := gp' and solve x^(k-1) ≡ y'x'^(-1) mod p' instead. See
        ;; https://math.stackexchange.com/questions/131127/ for the detail.
        (let ((g (gcd x divisor)))
          (declare ((integer 0 #.most-positive-fixnum) g))
          ;; Below is tha special treatment for the case x ≡ y. Without this
          ;; (mod-log 4 0 4) returns not 1 but 2.
          (when (= x y)
            (return-from mod-log 1))
          (multiple-value-bind (y-prime rem) (floor y g)
            (if (zerop rem)
                (let* ((x-prime (floor x g))
                       (p-prime (floor divisor g))
                       (next-rhs (mod (* y-prime (mod-inverse x-prime p-prime)) p-prime))
                       (res (mod-log x next-rhs p-prime)))
                  (declare ((integer 0 #.most-positive-fixnum) next-rhs))
                  (if res (+ 1 res) nil))
                nil))))))

(declaim (inline calc-min-factor))
(defun calc-min-factor (x alpha)
  "Returns k, so that x+k*alpha is the smallest non-negative number."
  (if (plusp alpha)
      (ceiling (- x) alpha)
      (floor (- x) alpha)))

(declaim (inline calc-max-factor))
(defun calc-max-factor (x alpha)
  "Returns k, so that x+k*alpha is the largest non-positive number."
  (if (plusp alpha)
      (floor (- x) alpha)
      (ceiling (- x) alpha)))

(defun solve-bezout (a b c &optional min max)
  "Returns an integer solution of a*x+b*y = c, if it exists.

If MIN is specified and MAX is null, the returned x is the smallest integer
equal or larger than MIN. If MAX is specified and MIN is null, x is the largest
integer equal or smaller than MAX. If the both are specified, x is an integer in
[MIN, MAX]. This function returns NIL when no x that satisfies the given
condition exists."
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
                      (let* ((k-min (calc-min-factor (- x0 min) deltax))
                             (x (+ x0 (* k-min deltax)))
                             (y (- y0 (* k-min deltay))))
                        (if (and max (> x max))
                            (values nil nil)
                            (values x y)))
                      (let* ((k-max (calc-max-factor (- x0 max) deltax))
                             (x (+ x0 (* k-max deltax)))
                             (y (- y0 (* k-max deltay))))
                        (if (<= x max)
                            (values x y)
                            (values nil nil))))))))
        (values nil nil))))

;; Reference: http://drken1215.hatenablog.com/entry/2019/03/20/202800
;; TODO: enable to deal with `a vector of vectors' as target
(declaim (inline mod-echelon))
(defun mod-echelon (matrix divisor &optional extended)
  "Returns the row echelon form of MATRIX by gaussian elimination.

This function destructively modifies MATRIX."
  (declare ((integer 1 #.most-positive-fixnum) divisor))
  (destructuring-bind (m n) (array-dimensions matrix)
    (declare ((integer 0 #.most-positive-fixnum) m n))
    (macrolet ((swap-rows (row1 row2)
                 `(dotimes (j n) (rotatef (aref matrix ,row1 j) (aref matrix ,row2 j)))))
      (dotimes (i m)
        (dotimes (j n)
          (setf (aref matrix i j) (mod (aref matrix i j) divisor))))
      (let ((rank 0))
        (dotimes (target-col (if extended (- n 1) n))
          (let ((pivot-row (do ((i rank (+ 1 i)))
                               ((= i m) -1)
                             (unless (zerop (aref matrix i target-col))
                               (return i)))))
            (when (>= pivot-row 0)
              (loop for j from target-col below n
                    do (rotatef (aref matrix rank j) (aref matrix pivot-row j)))
              (let ((inv (mod-inverse (aref matrix rank target-col) divisor)))
                (dotimes (j n)
                  (setf (aref matrix rank j)
                        (mod  (* inv (aref matrix rank j)) divisor)))
                (dotimes (i m)
                  (unless (or (= i rank) (zerop (aref matrix i target-col)))
                    (let ((factor (aref matrix i target-col)))
                      (loop for j from target-col below n
                            do (setf (aref matrix i j)
                                     (mod (- (aref matrix i j)
                                             (mod (* (aref matrix rank j) factor) divisor))
                                          divisor)))))))
              (incf rank))))
        (values matrix rank)))))
