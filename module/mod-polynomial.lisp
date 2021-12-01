(defpackage :cp/mod-polynomial
  (:use :cl :cp/mod-inverse)
  (:export #:poly-value #:poly-mult #:poly-floor! #:poly-mod! #:poly-div #:poly-div!
           #:poly-mod-power
           #:poly-differentiate! #:poly-integrate #:poly-shift! #:poly-scale!)
  (:documentation "Provides fundamental operations to polynomials over Z/nZ."))
(in-package :cp/mod-polynomial)

;; TODO: make another modules for non-modular polynomials, or introduce some
;; abstraction to integrate it

(declaim (inline poly-value))
(defun poly-value (p x modulus)
  "Evaluates P at X."
  (declare (vector p)
           (fixnum x)
           ((integer 1 #.most-positive-fixnum) modulus))
  (let ((res 0))
    (declare (fixnum res))
    (loop for i from (- (length p) 1) downto 0
          do (setq res (mod (+ (mod (* res x) modulus) (aref p i)) modulus)))
    res))

(declaim (inline poly-mult))
(defun poly-mult (p1 p2 modulus &optional result-vector)
  "Multiplies two polynomials P1(x) and P2(x) in O(deg(P1)deg(P2)) time.

The result is stored in RESULT-VECTOR if it is given, otherwise a new vector is
created."
  (declare (vector p1 p2)
           ((or null vector) result-vector)
           ((integer 1 #.most-positive-fixnum) modulus))
  (let* ((deg1 (- (length p1) 1))
         (deg2 (- (length p2) 1))
         (len (max 0 (+ deg1 deg2 1))))
    (declare ((integer -1 (#.array-dimension-limit)) deg1 deg2 len))
    (when (or (= -1 deg1) (= -1 deg2))
      (return-from poly-mult (make-array 0 :element-type (array-element-type p1))))
    (let ((res (or result-vector (make-array len :element-type (array-element-type p1)))))
      (declare ((integer -1 (#.array-dimension-limit)) len))
      (dotimes (d len res)
        ;; 0 <= i <= deg1, 0 <= j <= deg2
        (loop with coef of-type (integer 0 #.most-positive-fixnum) = 0
              for i from (max 0 (- d deg2)) to (min d deg1)
              for j = (- d i)
              do (setq coef (mod (+ coef (* (aref p1 i) (aref p2 j))) modulus))
              finally (setf (aref res d) coef))))))

(declaim (inline poly-div))
(defun poly-div (p1 p2 modulus &optional result-length)
  "Divides P1(x) by P2(x), regarding them as formal power series. The length of
the returned vector is the same as P1, when RESULT-LENGTH is null. Time
complexity is O(deg(P1)deg(P2))."
  (declare (vector p1 p2)
           ((or null (integer 0 #.most-positive-fixnum)) result-length)
           ((integer 1 #.most-positive-fixnum) modulus))
  (let* ((len1 (length p1))
         (len2 (+ 1 (or (position 0 p2 :from-end t :test-not #'eql)
                        (error 'division-by-zero
                               :operation #'poly-div
                               :operands (list p1 p2)))))
         (len (or result-length len1))
         (res (make-array len :element-type (array-element-type p1)))
         (inv (mod-inverse (aref p2 0) modulus)))
    (declare ((integer 0 #.most-positive-fixnum) len1 len2 len))
    (dotimes (i len res)
      (let ((coef (mod (* inv (- (if (< i len1) (aref p1 i) 0)
                                 (aref res i)))
                       modulus)))
        (setf (aref res i) coef)
        (loop for j from 1 below (min (- len i) len2)
              do (setf (aref res (+ i j))
                       (mod (+ (aref res (+ i j)) (* coef (aref p2 j))) modulus)))))))

(declaim (inline poly-div!))
(defun poly-div! (p1 p2 modulus)
  "Returns P1(x)/P2(x), regarding polynomial as formal power series. The result
is stored in P1. Time complexity is O(deg(P1)deg(P2))."
  (declare (vector p1 p2)
           ((integer 1 #.most-positive-fixnum) modulus))
  (let* ((len1 (length p1))
         (len2 (+ 1 (or (position 0 p2 :from-end t :test-not #'eql)
                        (error 'division-by-zero
                               :operation #'poly-div
                               :operands (list p1 p2)))))
         (inv (mod-inverse (aref p2 0) modulus)))
    (declare ((integer 0 #.most-positive-fixnum) len1 len2))
    (dotimes (i len1 p1)
      (let ((coef (mod (* inv (aref p1 i)) modulus)))
        (setf (aref p1 i) coef)
        (loop for j from 1 below (min (- len1 i) len2)
              do (setf (aref p1 (+ i j))
                       (mod (- (aref p1 (+ i j)) (* coef (aref p2 j))) modulus)))))))

;; Reference: http://web.cs.iastate.edu/~cs577/handouts/polydivide.pdf
(declaim (inline poly-floor!))
(defun poly-floor! (p1 p2 modulus &optional quotient)
  "Returns the quotient q(x) and the remainder r(x): P1(x) = q(x)P2(x) + r(x),
deg(r) < deg(P2). The time complexity is O((deg(P1)-deg(P2))deg(P2)).

The quotient is stored in QUOTIENT if it is given, otherwise a new vector is
created. This function doesn't modify P2.

Note that MODULUS and P2[deg(P2)] must be coprime."
  (declare (vector p1 p2)
           ((integer 1 #.most-positive-fixnum) modulus))
  (assert (not (or (eq p2 quotient) (eq p1 quotient))))
  ;; m := deg(p1), n := deg(p2)
  (let* ((m (or (position 0 p1 :from-end t :test-not #'eql) -1))
         (n (or (position 0 p2 :from-end t :test-not #'eql)
                (error 'division-by-zero
                       :operation #'poly-floor!
                       :operands (list p1 p2))))
         (quot (or quotient
                   (make-array (max 0 (+ 1 (- m n))) :element-type (array-element-type p1))))
         (inv (mod-inverse (aref p2 n) modulus)))
    (declare ((integer -1 (#.array-dimension-limit)) m n))
    (loop for k from (- m n) downto 0
          do (setf (aref quot k) (mod (* (aref p1 (+ n k)) inv) modulus))
             (loop for j from (+ n k -1) downto k
                   do (setf (aref p1 j)
                            (mod (- (aref p1 j) (* (aref quot k) (aref p2 (- j k))))
                                 modulus))))
    (let ((end (+ 1 (or (position 0 p1 :from-end t :test-not #'eql :end (min n (length p1))) -1))))
      (values quot (adjust-array p1 end)))))

(declaim (inline poly-mod!))
(defun poly-mod! (p divisor modulus)
  "Returns the remainder of P divided by DIVISOR over Z/nZ. This function
destructively modifies P. The time complexity is
O((deg(P)-deg(DIVISOR))deg(DIVISOR))."
  (declare (vector p divisor)
           ((integer 1 #.most-positive-fixnum) modulus))
  (let* ((m (or (position 0 p :from-end t :test-not #'eql) -1))
         (n (or (position 0 divisor :from-end t :test-not #'eql)
                (error 'division-by-zero
                       :operation #'poly-mod!
                       :operands (list p divisor))))
         (inv (mod-inverse (aref divisor n) modulus)))
    (declare ((integer -1 (#.array-dimension-limit)) m n))
    (loop for pivot-deg from m downto n
          for factor of-type (integer 0 #.most-positive-fixnum)
             = (mod (* (aref p pivot-deg) inv) modulus)
          do (loop for delta from 0 to n
                   do (setf (aref p (- pivot-deg delta))
                            (mod (- (aref p (- pivot-deg delta))
                                    (* factor (aref divisor (- n delta))))
                                 modulus))))
    (let ((end (+ 1 (or (position 0 p :from-end t :test-not #'eql) -1))))
      (adjust-array p end))))

(defun poly-mod-power (p exponent divisor modulus)
  "Returns P to the power of EXPONENT modulo DIVISOR."
  (declare (vector p divisor)
           ((integer 0 #.most-positive-fixnum) exponent)
           ((integer 1 #.most-positive-fixnum) modulus))
  (labels
      ((recur (power)
         (declare ((integer 0 #.most-positive-fixnum) power))
         (cond ((zerop power)
                (make-array 1 :element-type (array-element-type p) :initial-element 1))
               ((oddp power)
                (poly-mod! (poly-mult p (recur (- power 1)) modulus)
                           divisor modulus))
               ((let ((res (recur (floor power 2))))
                  (poly-mod! (poly-mult res res modulus)
                             divisor modulus))))))
    (recur exponent)))

(declaim (inline poly-differentiate!))
(defun poly-differentiate! (p modulus)
  "Returns the derivative of P."
  (declare (vector p)
           ((integer 1 #.most-positive-fixnum) modulus))
  (when (zerop (length p))
    (return-from poly-differentiate! p))
  (dotimes (i (- (length p) 1))
    (let ((coef (mod (the fixnum (* (aref p (+ i 1)) (+ i 1)))
                     modulus)))
      (declare ((integer 0 #.most-positive-fixnum) coef))
      (setf (aref p i) coef)))
  (let ((end (+ 1 (or (position 0 p :from-end t :end (- (length p) 1) :test-not #'=)
                      -1))))
    (adjust-array p end)))

(declaim (inline poly-integrate))
(defun poly-integrate (p modulus)
  "Returns an indefinite integral of P. Assumes the integration constant to
be zero."
  (declare (vector p)
           ((integer 1 #.most-positive-fixnum) modulus))
  (let* ((n (length p)))
    (when (zerop n)
      (return-from poly-integrate (make-array 0 :element-type (array-element-type p))))
    (let ((result (make-array (+ n 1) :element-type (array-element-type p) :initial-element 0)))
      (dotimes (i n)
        (setf (aref result (+ i 1))
              (mod (* (the fixnum (aref p i)) (mod-inverse (+ i 1) modulus)) modulus)))
      result)))

(declaim (inline poly-shift!))
(defun poly-shift! (p constant modulus)
  "Adds a constant to P."
  (declare (vector p)
           (fixnum constant)
           ((integer 1 #.most-positive-fixnum) modulus))
  (let ((n (length p))
        (constant (mod constant modulus)))
    (if (zerop n)
        (if (zerop constant)
            (make-array 0 :element-type (array-element-type p))
            (make-array 1 :element-type (array-element-type p) :initial-element constant))
        (progn
          (setf (aref p 0) (mod (+ (aref p 0) constant) modulus))
          (if (and (= n 1) (zerop (aref p 0)))
              (make-array 0 :element-type (array-element-type p))
              p)))))

(declaim (inline poly-scale!))
(defun poly-scale! (p constant modulus)
  "Returns a scalar multiplication of P."
  (declare (vector p)
           (fixnum constant)
           ((integer 1 #.most-positive-fixnum) modulus))
  (let ((constant (mod constant modulus)))
    (if (zerop constant)
        (make-array 0 :element-type (array-element-type p))
        (dotimes (i (length p) p)
          (setf (aref p i) (mod (* constant (the fixnum (aref p i))) modulus))))))
