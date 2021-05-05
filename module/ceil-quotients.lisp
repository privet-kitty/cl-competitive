(defpackage :cp/ceil-quotients
  (:use :cl)
  (:export #:make-cquot-manager #:cquot-length #:cquot-n #:cquot-long-p #:cquot-csqrt
           #:cquot-index #:cquot-get #:enum-cquotients #:map-cquotients))
(in-package :cp/ceil-quotients)

;; TODO: more tests and docs

(defstruct (cquot-manager (:constructor %make-cquot-manager)
                          (:conc-name cquot-))
  (n nil :type (integer 0 #.most-positive-fixnum))
  (length nil :type (integer 0 #.most-positive-fixnum))
  (long-p nil :type boolean)
  (csqrt nil :type (integer 0 #.most-positive-fixnum)))

(defun make-cquot-manager (n)
  (declare (optimize (speed 3))
           ((integer 1 #.most-positive-fixnum) n))
  (let* ((csqrt (+ 1 (isqrt (- n 1))))
         (long-p (> n (* csqrt (- csqrt 1))))
         (length (+ (* 2 (- csqrt 1))
                    (if long-p 1 0))))
    (%make-cquot-manager :n n :length length :long-p long-p :csqrt csqrt)))

(declaim (inline cquot-index))
(defun cquot-index (cquot x)
  "Returns the position of x on the sequence of quotients."
  (declare ((integer 1 #.most-positive-fixnum) x))
  (if (<= x (cquot-csqrt cquot))
      (- x 1)
      (- (cquot-length cquot)
         (ceiling (cquot-n cquot) x))))

(declaim (inline cquot-get))
(defun cquot-get (cquot i)
  "Returns the i-th element of the sequence of quotients."
  (declare ((integer 0 #.most-positive-fixnum) i))
  (let ((csqrt (cquot-csqrt cquot)))
    (cond ((< (+ i 1) csqrt)
           (+ i 1))
          ((and (cquot-long-p cquot)
                (= (+ i 1) csqrt))
           csqrt)
          (t
           (let ((length (cquot-length cquot)))
             (assert (< i length))
             (ceiling (cquot-n cquot) (- length i)))))))

(declaim (ftype (function * (values (simple-array (integer 0 #.most-positive-fixnum) (*))
                                    &optional))
                enum-cquotients))
(defun enum-cquotients (n)
  "Characteristics:
- ceil(n/result[i]) < ceil(n/result[i-1])
- ceil(n/result[i]) == result[-i]"
  (declare (optimize (speed 3))
           ((integer 0 #.most-positive-fixnum) n))
  (let* ((sqrt (+ 1 (isqrt (- n 1))))
         (long-p (> n (* sqrt (- sqrt 1))))
         (length (+ (* 2 (- sqrt 1))
                    (if long-p 1 0)))
         (res (make-array length :element-type '(integer 0 #.most-positive-fixnum)))
         (end 0))
    (declare ((integer 0 #.most-positive-fixnum) sqrt length end))
    (labels ((set! (x)
               (setf (aref res end) x)
               (incf end)))
      (loop for i from 1 below sqrt
            do (set! i))
      (when (> n (* (- sqrt 1) sqrt))
        (set! sqrt))
      (loop for i from (- sqrt 1) downto 1
            do (set! (ceiling n i))))
    res))

(declaim (inline map-cquotients))
(defun map-cquotients (function n)
  (declare ((integer 0 #.most-positive-fixnum) n))
  (let ((sqrt (+ 1 (isqrt (- n 1)))))
    (declare ((integer 0 #.most-positive-fixnum) sqrt))
    (loop for i from 1 below sqrt
          do (funcall function i))
    (when (> n (* (- sqrt 1) sqrt))
      (funcall function sqrt))
    (loop for i from (- sqrt 1) downto 1
          do (funcall function (ceiling n i)))))
