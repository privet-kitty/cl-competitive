(defpackage :cp/lagrange-interpolation
  (:use :cl :cp/mod-inverse)
  (:export #:calc-lagrange-base
           #:lagrange-interpolation))
(in-package :cp/lagrange-interpolation)

;; NOTE & TODO: not optimized

(defun calc-lagrange-base (args values modulus &key (element-type '(unsigned-byte 31)))
  "Returns a vector of coefficients of lagrange base: vector[k] is equal to the
coefficient of (x-a_1)(x-a_2)...(x-a_n)/(x-a_k). Let f(x) is a polynomial what
we want to know. Then it must hold f(ARGS[i]) = VALUES[i] for all i.

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

(defun lagrange-interpolation (args values modulus &key (element-type '(unsigned-byte 31)))
  "Does polynomial interpolation and returns a vector of coefficients: vector[k]
:= coefficient of x^k."
  (let* ((n (length values))
         (base (calc-lagrange-base args values modulus :element-type element-type))
         (dp (make-array (list (+ n 1) n) :element-type element-type :initial-element 0))
         (result (make-array n :element-type element-type :initial-element 0))
         (tmp (make-array n :element-type element-type :initial-element 0)))
    (when (zerop n)
      (return-from lagrange-interpolation result))
    ;; KLUDGE: this kind of error should be signalled in MOD-INVERSE.
    (dotimes (i n)
      (assert (= 1 (gcd modulus (aref args i)))))
    (setf (aref dp 0 0) 1)
    (loop for i from 1 to n
          for arg = (aref args (- i 1))
          do (setf (aref dp i 0)
                   (mod (* (- arg) (aref dp (- i 1) 0)) modulus))
             (loop for j from 1 below n
                   do (setf (aref dp i j)
                            (mod (- (aref dp (- i 1) (- j 1))
                                    (mod (* arg (aref dp (- i 1) j)) modulus))
                                 modulus))))
    (dotimes (k n)
      (let* ((arg (aref args k))
             (/arg (mod-inverse arg modulus)))
        (setf (aref tmp 0) (mod (* (aref dp n 0) (- /arg)) modulus))
        (loop for j from 1 below n
              do (setf (aref tmp j)
                       (mod (* (mod (- (aref tmp (- j 1)) (aref dp n j)) modulus)
                               /arg)
                            modulus)))
        (let ((coef (aref base k)))
          (dotimes (i n)
            (setf (aref result i)
                  (mod (+ (aref result i)
                          (mod (* coef (aref tmp i)) modulus))
                       modulus))))))
    result))
