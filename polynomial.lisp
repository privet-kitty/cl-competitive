;; NOTE: These are poor man's utilities for polynomial arithmetic. NOT
;; sufficiently equipped in all senses.

(declaim (inline poly-value))
(defun poly-value (poly x modulus)
  "Returns the value f(x)."
  (declare (vector poly))
  (let ((x^i 1)
        (res 0))
    (declare (fixnum x^i res))
    (dotimes (i (length poly))
      (setq res (mod (+ res (* x^i (aref poly i))) modulus))
      (setq x^i (mod (* x^i x) modulus)))
    res))

;; naive multiplication
(declaim (inline poly-mult))
(defun poly-mult (u v modulus &optional result-vector)
  "Multiplies u(x) and v(x) on Z/nZ in O(deg(u)deg(v)) time.

The result is stored in RESULT-VECTOR if it is given, otherwise a new vector is
created."
  (declare (vector u v)
           ((or null vector) result-vector)
           ((integer 1 #.most-positive-fixnum) modulus))
  (let* ((deg1 (loop for i from (- (length u) 1) downto 0
                     while (zerop (aref u i))
                     finally (return i)))
         (deg2 (loop for i from (- (length v) 1) downto 0
                     while (zerop (aref v i))
                     finally (return i)))
         (len (max 0 (+ deg1 deg2 1)))
         (res (or result-vector (make-array len :element-type (array-element-type u)))))
    (declare ((integer -1 (#.array-total-size-limit)) deg1 deg2 len))
    (dotimes (d len res)
      ;; 0 <= i <= deg1, 0 <= j <= deg2
      (loop with coef of-type (integer 0 #.most-positive-fixnum) = 0
            for i from (max 0 (- d deg2)) to (min d deg1)
            for j = (- d i)
            do (setq coef (mod (+ coef (mod (* (aref u i) (aref v j)) modulus))
                               modulus))
            finally (setf (aref res d) coef)))))

(declaim (ftype (function * (values (mod #.most-positive-fixnum) &optional)) %mod-inverse))
(defun %mod-inverse (a modulus)
  "Solves ax ≡ 1 mod m. A and M must be coprime."
  (declare (optimize (speed 3))
           (integer a)
           ((integer 1 #.most-positive-fixnum) modulus))
  (labels ((%gcd (a b)
             (declare (optimize (safety 0))
                      ((integer 0 #.most-positive-fixnum) a b))
             (if (zerop b)
                 (values 1 0)
                 (multiple-value-bind (p q) (floor a b) ; a = pb + q
                   (multiple-value-bind (v u) (%gcd b q)
                     (declare (fixnum u v))
                     (values u (the fixnum (- v (the fixnum (* p u))))))))))
    (mod (%gcd (mod a modulus) modulus) modulus)))

;; Naive division
;; Reference: http://web.cs.iastate.edu/~cs577/handouts/polydivide.pdf
(declaim (inline poly-floor!))
(defun poly-floor! (u v modulus &optional quotient)
  "Returns the quotient q(x) and the remainder r(x) on Z/nZ: u(x) = q(x)v(x) + r(x),
deg(r) < deg(v). This function destructively modifies U. The time complexity is
O((deg(u)-deg(v))deg(v)).

The quotient is stored in QUOTIENT if it is given, otherwise a new vector is
created.

Note that MODULUS and V[deg(V)] must be coprime."
  (declare (vector u v)
           ((integer 1 #.most-positive-fixnum) modulus))
  ;; m := deg(u), n := deg(v)
  (let* ((m (loop for i from (- (length u) 1) downto 0
                  while (zerop (aref u i))
                  finally (return i)))
         (n (loop for i from (- (length v) 1) downto 0
                  unless (zerop (aref v i))
                  do (return i)
                  finally (error 'division-by-zero
                                 :operation #'poly-floor!
                                 :operands (list u v))))
         (quot (or quotient
                   (make-array (max 0 (+ 1 (- m n)))
                               :element-type (array-element-type u))))
         ;; FIXME: Is it better to signal an error in non-coprime case?
         (inv (%mod-inverse (aref v n) modulus)))
    (declare ((integer -1 (#.array-total-size-limit)) m n))
    (loop for k from (- m n) downto 0
          do (setf (aref quot k)
                   (mod (* (aref u (+ n k)) inv) modulus))
             (loop for j from (+ n k -1) downto k
                   do (setf (aref u j)
                            (mod (- (aref u j)
                                    (mod (* (aref quot k) (aref v (- j k))) modulus))
                                 modulus))))
    (loop for i from (- (length u) 1) downto n
          do (setf (aref u i) 0)
          finally (return (values quot u)))))

;; Naive division
(defun poly-mod! (poly divisor modulus)
  "Returns the remainder of POLY divided by DIVISOR on Z/nZ. This function
destructively modifies POLY."
  (declare (vector poly divisor)
           ((integer 1 #.most-positive-fixnum) modulus))
  (let* ((m (loop for i from (- (length poly) 1) downto 0
                  while (zerop (aref poly i))
                  finally (return i)))
         (n (loop for i from (- (length divisor) 1) downto 0
                  unless (zerop (aref divisor i))
                  do (return i)
                  finally (error 'division-by-zero
                                 :operation #'poly-mod!
                                 :operands (list poly divisor))))
         (inv (%mod-inverse (aref divisor n) modulus)))
    (declare ((integer -1 (#.array-total-size-limit)) m n))
    (loop for pivot-deg from m downto n
          for factor of-type (integer 0 #.most-positive-fixnum)
             = (mod (* (aref poly pivot-deg) inv) modulus)
          do (loop for delta from 0 to n
                   do (setf (aref poly (- pivot-deg delta))
                            (mod
                             (- (aref poly (- pivot-deg delta))
                                (mod (* factor (aref divisor (- n delta)))
                                     modulus))
                             modulus))))
    poly))

(defun poly-power (poly exponent divisor modulus)
  "Returns POLY to the power of EXPONENT modulo DIVISOR on Z/nZ."
  (labels
      ((recur (power)
         (declare ((integer 0 #.most-positive-fixnum) power))
         (cond ((zerop power)
                (make-array 1 :element-type (array-element-type poly) :initial-element 1))
               ((oddp power)
                (poly-mod! (poly-mult poly (recur (- power 1)) modulus)
                           divisor modulus))
               ((let ((subpoly (recur (floor power 2))))
                  (poly-mod! (poly-mult subpoly subpoly modulus)
                             divisor modulus))))))
    (recur exponent)))
