(defpackage :cp/integer-log
  (:use :cl)
  (:export #:log2-ceil #:log-ceil #:log10-floor #:decimal-length))
(in-package :cp/integer-log)

(declaim (inline log2-ceil))
(defun log2-ceil (x)
  "Rounds up log2(x). Special case: (log2-ceil 0) = 0"
  (declare ((real 0) x))
  (integer-length (- (ceiling x) 1)))

(declaim (inline log-ceil))
(defun log-ceil (x base)
  "Rounds up log(x). Signals DIVISION-BY-ZERO if X is zero."
  (declare (real x)
           ((integer 2) base))
  (when (zerop x)
    (error 'division-by-zero :operands (list 0 base) :operation 'log-ceil))
  (if (integerp x)
      (loop for i from 0
            for y = x then (ceiling y base)
            when (<= y 1)
            do (return i))
      (nth-value 0 (ceiling (log x base)))))

(defconstant +word-bits+ 62)
(deftype uint () '(unsigned-byte #.+word-bits+))

(declaim ((simple-array (unsigned-byte 8) (*)) *lo*))
(sb-ext:define-load-time-global *lo*
  (let ((tmp (make-array (+ 1 +word-bits+) :element-type '(unsigned-byte 8))))
    (dotimes (exp (length tmp) tmp)
      (setf (aref tmp exp)
            (- (length (write-to-string (ash 1 (- exp 1)))) 1)))))

(declaim ((simple-array uint (*)) *hi-power10*))
(sb-ext:define-load-time-global *hi-power10*
  (let* ((max (reduce #'max *lo*))
         (tmp (make-array (+ max 1) :element-type 'uint)))
    (dotimes (i (length tmp) tmp)
      (setf (aref tmp i)
            (min (- (expt 10 (+ i 1)) 1)
                 (- (ash 1 +word-bits+) 1))))))

(declaim (inline log10-floor))
(defun log10-floor (x)
  "Returns floor(log_{10}(x)). Special case: (log10-floor 0) == 0."
  (declare (uint x))
  (let ((lo (aref *lo* (integer-length x))))
    (+ lo (if (> x (aref *hi-power10* lo)) 1 0))))

(declaim (inline decimal-length))
(defun decimal-length (x)
  "Special case (decimal-length 0) == 1."
  (declare (uint x))
  (+ 1 (log10-floor x)))
