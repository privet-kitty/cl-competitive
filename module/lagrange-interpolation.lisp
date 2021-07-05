(defpackage :cp/lagrange-interpolation
  (:use :cl :cp/mod-inverse)
  (:export #:calc-lagrange-base
           #:lagrange-interpolation))
(in-package :cp/lagrange-interpolation)

;; NOTE & TODO: not optimized

(declaim (inline calc-lagrange-base))
(defun calc-lagrange-base (args values modulus &key (element-type '(unsigned-byte 31)))
  "Returns a vector of coefficients of lagrange base: vector[k] is equal to the
coefficient of (x-a_1)(x-a_2)...(x-a_n)/(x-a_k). Let f(x) be the polynomial what
we want to know. Then f(ARGS[i]) = VALUES[i] must hold for all i.

MODULUS must be coprime with any integers that appear in computation."
  (declare (vector args values)
           ((integer 1 #.most-positive-fixnum) modulus))
  (let* ((n (length args))
         (result (make-array n :element-type element-type)))
    (assert (= n (length values)))
    (dotimes (i n)
      (let ((arg (aref args i))
            (value (aref values i))
            (denom 1))
        (declare ((integer 0 #.most-positive-fixnum) denom)
                 (fixnum arg value))
        (dotimes (j n)
          (unless (= i j)
            (setq denom (mod (* denom (- arg (the fixnum (aref args j)))) modulus))))
        (setf (aref result i)
              (mod (* value (mod-inverse denom modulus)) modulus))))
    result))

(declaim (inline lagrange-interpolation))
(defun lagrange-interpolation (args values modulus &key (element-type '(unsigned-byte 31)))
  "Does polynomial interpolation and returns a vector of coefficients: vector[k]
:= coefficient of x^k. ARGS may not have duplicate values."
  (declare (vector args values)
           ((integer 1 #.most-positive-fixnum) modulus))
  (labels ((mod- (x y) (mod (- x y) modulus))
           (mod+ (x y) (mod (+ x y) modulus)))
    (let* ((n (length values))
           (base (calc-lagrange-base args values modulus :element-type element-type))
           (dp (make-array n :element-type element-type :initial-element 0))
           (result (make-array n :element-type element-type :initial-element 0))
           (tmp (make-array n :element-type element-type :initial-element 0)))
      (when (zerop n)
        (return-from lagrange-interpolation result))
      (setf (aref dp 0) (mod 1 modulus))
      (loop for i from 1 to n
            for arg = (aref args (- i 1))
            do (loop for j from (- n 1) downto 1
                     do (setf (aref dp j)
                              (mod- (aref dp (- j 1))
                                    (mod (* arg (aref dp j)) modulus))))
               (setf (aref dp 0)
                     (mod (* (- arg) (aref dp 0)) modulus)))
      (dotimes (k n)
        (let* ((arg (aref args k)))
          (declare (fixnum arg))
          (if (zerop arg)
              ;; special treatment to avoid division by zero
              (loop for j from 0 below (- n 1)
                    do (setf (aref tmp j) (aref dp (+ j 1)))
                    finally (setf (aref tmp (- n 1)) (mod 1 modulus)))
              (let ((/arg (mod-inverse arg modulus)))
                (setf (aref tmp 0) (mod (* (aref dp 0) (- /arg)) modulus))
                (loop for j from 1 below n
                      do (setf (aref tmp j)
                               (mod (* /arg (mod- (aref tmp (- j 1)) (aref dp j)))
                                    modulus)))))
          (let ((coef (aref base k)))
            (dotimes (j n)
              (setf (aref result j)
                    (mod+ (aref result j)
                          (mod (* coef (aref tmp j)) modulus)))))))
      result)))

