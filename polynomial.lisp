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
(defun poly-mult (a b modulus &optional result-vector)
  "Convolutes A and B on Z/nZ in O(deg(A)deg(B)) time.

The result is stored in RESULT-VECTOR if it is given, otherwise a new vector is
created."
  (declare (vector a b)
           ((or null vector) result-vector)
           ((integer 1 #.most-positive-fixnum) modulus))
  (assert (and (not (eq b result-vector)) (not (eq a result-vector))))
  (let* ((len1 (length a))
         (len2 (length b))
         (len (max 0 (+ len1 len2 -1)))
         (res (or result-vector (make-array len :element-type (array-element-type a)))))
    (declare ((integer 0 (#.array-total-size-limit)) len1 len2 len))
    (dotimes (d len res)
      ;; 0 <= i <= deg1 (= len1 -1), 0 <= j <= deg2 (= len2 - 1)
      (loop with coef of-type (integer 0 #.most-positive-fixnum) = 0
            for i from (max 0 (- d (- len2 1))) to (min d (- len1 1))
            for j = (- d i)
            do (setq coef (mod (+ coef (mod (* (aref a i) (aref b j)) modulus))
                               modulus))
            finally (setf (aref res d) coef)))))

;; Naive division
;; TODO: integrate it into poly-floor!
(defun poly-mod (poly pdivisor modulus)
  "Returns the remainder of POLY divided by PDIVISOR on Z/nZ."
  (declare (vector poly pdivisor)
           ((integer 1 #.most-positive-fixnum) modulus))
  (assert (>= (length pdivisor) 1))
  (if (< (length poly) (length pdivisor))
      poly
      (let* ((source-deg (- (length poly) 1))
             (pdivisor-deg (- (length pdivisor) 1))
             (result (make-array (length poly)
                                 :initial-contents poly
                                 :element-type (array-element-type poly))))
        (loop for base-deg from source-deg downto pdivisor-deg
              for factor of-type fixnum
                 = (floor (aref result base-deg) (aref pdivisor pdivisor-deg))
              do (loop for delta from 0 to pdivisor-deg
                       do (setf (aref result (- base-deg delta))
                                (mod
                                 (- (aref result (- base-deg delta))
                                    (* factor (aref pdivisor (- pdivisor-deg delta))))
                                 modulus))))
        ;; Adjusts the RESULT to the actual degree.
        (loop for valid-deg from pdivisor-deg downto 0
              while (zerop (aref result valid-deg))
              finally (return (adjust-array result (+ 1 valid-deg)))))))

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
(defun poly-floor! (u v modulus &optional quotient)
  "Returns the quotient q(x) and the remainder r(x) on Z/nZ: u(x) = q(x)v(x) + r(x),
deg(r) < deg(v). This function destructively modifies U. The time complexity is
O((deg(u)-deg(v))deg(v)).

The quotient is stored in QUOTIENT if it is given, otherwise a new vector is
created.

Note that MODULUS and V[deg(V)] must be coprime."
  (declare (vector u v)
           ((integer 1 #.most-positive-fixnum) modulus))
  ;; (assert (and (>= (length u) 1) (>= (length v) 1)))
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
                               :element-type (array-element-type u)))))
    (declare ((integer -1 (#.array-total-size-limit)) m n))
    (loop for k from (- m n) downto 0
          do (setf (aref quot k)
                   (mod (* (aref u (+ n k))
                           ;; FIXME: better to signal an error in non-coprime case?
                           (%mod-inverse (aref v n) modulus))
                        modulus))
             (loop for j from (+ n k -1) downto k
                   do (setf (aref u j)
                            (mod (- (aref u j)
                                    (mod (* (aref quot k) (aref v (- j k))) modulus))
                                 modulus))))
    (loop for i from (- (length u) 1) downto n
          do (setf (aref u i) 0)
          finally (return (values quot u)))))

(defun poly-power (poly exponent pdivisor modulus)
  "Returns POLY to the power of EXPONENT modulo PDIVISOR on Z/nZ."
  (labels
      ((recur (power)
         (declare ((integer 0 #.most-positive-fixnum) power))
         (cond ((zerop power)
                (make-array 1 :element-type (array-element-type poly) :initial-element 1))
               ((oddp power)
                (poly-mod (poly-mult poly (recur (- power 1)) modulus)
                          pdivisor modulus))
               ((let ((subpoly (recur (floor power 2))))
                  (poly-mod (poly-mult subpoly subpoly modulus)
                            pdivisor modulus))))))
    (recur exponent)))
