;;;
;;; Binomial coefficient with mod
;;; build: O(n)
;;; query: O(1)
;;;

(defpackage :cp/binomial-coefficient-mod
  (:use :cl)
  (:export #:binom #:perm #:multinomial #:stirling2 #:catalan #:+binom-mod+
           #:*fact* #:*fact-inv* #:*inv*))
(in-package :cp/binomial-coefficient-mod)

;; TODO: non-global handling

(defconstant +binom-size+ 510000)
(defconstant +binom-mod+ (if (boundp 'cl-user::+mod+)
                             (symbol-value 'cl-user::+mod+)
                             #.(+ (expt 10 9) 7)))

(defparameter *fact* (make-array +binom-size+ :element-type '(unsigned-byte 31))
  "table of factorials")
(defparameter *fact-inv* (make-array +binom-size+ :element-type '(unsigned-byte 31))
  "table of inverses of factorials")
(defparameter *inv* (make-array +binom-size+ :element-type '(unsigned-byte 31))
  "table of inverses of non-negative integers")
(declaim ((simple-array (unsigned-byte 31) (*)) *fact* *fact-inv* *inv*))

(defun initialize-binom ()
  (declare (optimize (speed 3) (safety 0)))
  (setf (aref *fact* 0) 1
        (aref *fact* 1) 1
        (aref *fact-inv* 0) 1
        (aref *fact-inv* 1) 1
        (aref *inv* 1) 1)
  (loop for i from 2 below +binom-size+
        do (setf (aref *fact* i) (mod (* i (aref *fact* (- i 1))) +binom-mod+)
                 (aref *inv* i) (- +binom-mod+
                                   (mod (* (aref *inv* (rem +binom-mod+ i))
                                           (floor +binom-mod+ i))
                                        +binom-mod+))
                 (aref *fact-inv* i) (mod (* (aref *inv* i)
                                             (aref *fact-inv* (- i 1)))
                                          +binom-mod+))))

(initialize-binom)

(declaim (inline binom))
(defun binom (n k)
  "Returns nCk."
  (if (or (< n k) (< n 0) (< k 0))
      0
      (mod (* (aref *fact* n)
              (mod (* (aref *fact-inv* k) (aref *fact-inv* (- n k))) +binom-mod+))
           +binom-mod+)))

(declaim (inline perm))
(defun perm (n k)
  "Returns nPk."
  (if (or (< n k) (< n 0) (< k 0))
      0
      (mod (* (aref *fact* n) (aref *fact-inv* (- n k))) +binom-mod+)))

;; TODO: compiler macro or source-transform
(declaim (inline multinomial))
(defun multinomial (&rest ks)
  "Returns the multinomial coefficient K!/k_1!k_2!...k_n! for K = k_1 + k_2 +
... + k_n. K must be equal to or smaller than
MOST-POSITIVE-FIXNUM. (multinomial) returns 1."
  (let ((sum 0)
        (result 1))
    (declare ((integer 0 #.most-positive-fixnum) result sum))
    (dolist (k ks)
      (incf sum k)
      (setq result
            (mod (* result (aref *fact-inv* k)) +binom-mod+)))
    (mod (* result (aref *fact* sum)) +binom-mod+)))

(declaim (inline stirling2))
(defun stirling2 (n k)
  "Returns the stirling number of the second kind S2(n, k). Time complexity is
O(klog(n))."
  (declare ((integer 0 #.most-positive-fixnum) n k))
  (labels ((mod-power (base exp)
             (declare ((integer 0 #.most-positive-fixnum) base exp))
             (loop with res of-type (integer 0 #.most-positive-fixnum) = 1
                   while (> exp 0)
                   when (oddp exp)
                   do (setq res (mod (* res base) +binom-mod+))
                   do (setq base (mod (* base base) +binom-mod+)
                            exp (ash exp -1))
                   finally (return res))))
    (loop with result of-type fixnum = 0
          for i from 0 to k
          for delta = (mod (* (binom k i) (mod-power i n)) +binom-mod+)
          when (evenp (- k i))
          do (incf result delta)
             (when (>= result +binom-mod+)
               (decf result +binom-mod+))
          else
          do (decf result delta)
             (when (< result 0)
               (incf result +binom-mod+))
          finally (return (mod (* result (aref *fact-inv* k)) +binom-mod+)))))

(declaim (inline catalan))
(defun catalan (n)
  "Returns the N-th Catalan number."
  (declare ((integer 0 #.most-positive-fixnum) n))
  (mod (* (aref *fact* (* 2 n))
          (mod (* (aref *fact-inv* (+ n 1))
                  (aref *fact-inv* n))
               +binom-mod+))
       +binom-mod+))
