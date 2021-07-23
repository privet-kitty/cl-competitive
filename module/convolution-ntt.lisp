(defpackage :cp/convolution-ntt
  (:use :cl :cp/ntt)
  (:export #:convolve #:convolution-int #:convolution-vector #:mod-convolve)
  (:documentation "Provides integer convolution within [-2^63, 2^63).

Reference:
https://github.com/atcoder/ac-library/blob/master/atcoder/convolution.hpp"))
(in-package :cp/convolution-ntt)

(deftype convolution-int () '(signed-byte 64))
(deftype convolution-vector () '(simple-array convolution-int (*)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun inv-gcd (a b)
    (declare (convolution-int a b))
    (let ((a (mod a b)))
      (if (zerop a)
          (values b 0)
          (let ((s b)
                (tt a)
                (m0 0)
                (m1 1))
            (loop until (zerop tt)
                  for u = (truncate s tt)
                  do (decf s (* tt u))
                     (decf m0 (* m1 u))
                     (rotatef s tt)
                     (rotatef m0 m1))
            (when (< m0 0)
              (incf m0 (truncate b s)))
            (values s m0)))))

  (defconstant +mod1+ 754974721)
  (defconstant +mod2+ 167772161)
  (defconstant +mod3+ 469762049)

  (defconstant +m2m3+ (* +mod2+ +mod3+))
  (defconstant +m1m3+ (* +mod1+ +mod3+))
  (defconstant +m1m2+ (* +mod1+ +mod2+))
  (defconstant +m1m2m3+ (ldb (byte 64 0) (* +mod1+ +mod2+ +mod3+)))

  (defconstant +i1+ (nth-value 1 (inv-gcd +m2m3+ +mod1+)))
  (defconstant +i2+ (nth-value 1 (inv-gcd +m1m3+ +mod2+)))
  (defconstant +i3+ (nth-value 1 (inv-gcd +m1m2+ +mod3+))))

(define-ntt +mod1+
  :ntt ntt1
  :inverse-ntt intt1
  :convolve convolve1)
(define-ntt +mod2+
  :ntt ntt2
  :inverse-ntt intt2
  :convolve convolve2)
(define-ntt +mod3+
  :ntt ntt3
  :inverse-ntt intt3
  :convolve convolve3)

(declaim (inline ensure-ntt-vector))
(defun ensure-ntt-vector (vector mod)
  (declare (vector vector))
  (let* ((len (length vector))
         (res (make-array len :element-type '(unsigned-byte 31))))
    (dotimes (i len)
      (setf (aref res i) (mod (aref vector i) mod)))
    res))

;; TODO: deal with negative number
(declaim (ftype (function * (values convolution-vector &optional)) convolve))
(defun convolve (vector1 vector2)
  "Does non-mod convolution. This function is non-destructive."
  (declare (optimize (speed 3))
           (vector vector1 vector2))
  (let ((n (length vector1))
        (m (length vector2)))
    (when (or (zerop n) (zerop m))
      (return-from convolve (make-array 0 :element-type 'convolution-int)))
    (let* ((vector1 (coerce vector1 'convolution-vector))
           (vector2 (coerce vector2 'convolution-vector))
           (c1 (convolve1 (ensure-ntt-vector vector1 +mod1+)
                          (ensure-ntt-vector vector2 +mod1+)))
           (c2 (convolve2 (ensure-ntt-vector vector1 +mod2+)
                          (ensure-ntt-vector vector2 +mod2+)))
           (c3 (convolve3 (ensure-ntt-vector vector1 +mod3+)
                          (ensure-ntt-vector vector2 +mod3+)))
           (result (make-array (+ n m -1) :element-type 'convolution-int)))
      (dotimes (i (+ n m -1))
        (let* ((x (ldb (byte 64 0)
                       (+ (* (mod (* (aref c1 i) +i1+) +mod1+) +m2m3+)
                          (* (mod (* (aref c2 i) +i2+) +mod2+) +m1m3+)
                          (* (mod (* (aref c3 i) +i3+) +mod3+) +m1m2+))))
               (diff (- (aref c1 i) (mod x +mod1+)))
               (diff (if (< diff 0) (+ diff +mod1+) diff)))
          (declare ((integer 0 #.most-positive-fixnum) diff)
                   ((unsigned-byte 64) x))
          (let ((offset #.(coerce (vector 0 0 +m1m2m3+ (* 2 +m1m2m3+) (* 3 +m1m2m3+))
                                  '(simple-array (integer 0 #.most-positive-fixnum) (*)))))
            (setf (aref result i) (- x (aref offset (mod diff 5)))))))
      result)))

(defun mod-convolve (vector1 vector2 modulus)
  "Does convolution on given mod. This function is non-destructive. Beware of
overflow."
  (declare (optimize (speed 3))
           ((integer 1 #.most-positive-fixnum) modulus))
  (let* ((mult (convolve vector1 vector2))
         (result (make-array (length mult) :element-type 'ntt-int)))
    (dotimes (i (length mult) result)
      (setf (aref result i) (mod (aref mult i) modulus)))))
