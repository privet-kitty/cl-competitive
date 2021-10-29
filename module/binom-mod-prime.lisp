(defpackage :cp/binom-mod-prime
  (:use :cl :cp/static-mod)
  (:export #:binom #:perm #:multinomial #:stirling2 #:catalan #:multichoose
           #:*fact* #:*fact-inv* #:*inv*)
  (:documentation
   "Provides tables of factorials, inverses, inverses ot factorials etc.
modulo prime.

build: O(n)
query: O(1)
"))
(in-package :cp/binom-mod-prime)

;; TODO: non-global handling
(defconstant +binom-size+ 510000)

(declaim ((simple-array (unsigned-byte 31) (*)) *fact* *fact-inv* *inv*))
(sb-ext:define-load-time-global *fact*
  (make-array +binom-size+ :element-type '(unsigned-byte 31))
  "table of factorials")
(sb-ext:define-load-time-global *fact-inv*
  (make-array +binom-size+ :element-type '(unsigned-byte 31))
  "table of inverses of factorials")
(sb-ext:define-load-time-global *inv*
  (make-array +binom-size+ :element-type '(unsigned-byte 31))
  "table of inverses of non-negative integers")

(defun initialize-binom ()
  (declare (optimize (speed 3) (safety 0)))
  (setf (aref *fact* 0) 1
        (aref *fact* 1) 1
        (aref *fact-inv* 0) 1
        (aref *fact-inv* 1) 1
        (aref *inv* 1) 1)
  (loop for i from 2 below +binom-size+
        do (setf (aref *fact* i) (mod (* i (aref *fact* (- i 1))) +mod+)
                 (aref *inv* i) (- +mod+
                                   (mod (* (aref *inv* (rem +mod+ i))
                                           (floor +mod+ i))
                                        +mod+))
                 (aref *fact-inv* i) (mod (* (aref *inv* i)
                                             (aref *fact-inv* (- i 1)))
                                          +mod+))))

(initialize-binom)

(declaim (inline binom))
(defun binom (n k)
  "Returns nCk, the number of k-combinations of n things without repetition."
  (if (or (< n k) (< n 0) (< k 0))
      0
      (mod (* (aref *fact* n)
              (mod (* (aref *fact-inv* k) (aref *fact-inv* (- n k))) +mod+))
           +mod+)))

(declaim (inline perm))
(defun perm (n k)
  "Returns nPk, the number of k-permutations of n things without repetition."
  (if (or (< n k) (< n 0) (< k 0))
      0
      (mod (* (aref *fact* n) (aref *fact-inv* (- n k))) +mod+)))

(declaim (inline multichoose))
(defun multichoose (n k)
  "Returns the number of k-combinations of n things with repetition."
  (binom (+ n k -1) k))

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
            (mod (* result (aref *fact-inv* k)) +mod+)))
    (mod (* result (aref *fact* sum)) +mod+)))

(define-compiler-macro multinomial (&rest args)
  (case (length args)
    ((0 1) (mod 1 +mod+))
    (otherwise
     `(mod (* ,(reduce (lambda (x y) `(mod (* ,x ,y) +mod+))
                       args
                       :key (lambda (x) `(aref *fact-inv* ,x)))
              (aref *fact* (+ ,@args)))
           +mod+))))

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
                   do (setq res (mod (* res base) +mod+))
                   do (setq base (mod (* base base) +mod+)
                            exp (ash exp -1))
                   finally (return res))))
    (loop with result of-type fixnum = 0
          for i from 0 to k
          for delta = (mod (* (binom k i) (mod-power i n)) +mod+)
          when (evenp (- k i))
          do (incf result delta)
             (when (>= result +mod+)
               (decf result +mod+))
          else
          do (decf result delta)
             (when (< result 0)
               (incf result +mod+))
          finally (return (mod (* result (aref *fact-inv* k)) +mod+)))))

(declaim (inline catalan))
(defun catalan (n)
  "Returns the N-th Catalan number."
  (declare ((integer 0 #.most-positive-fixnum) n))
  (mod (* (aref *fact* (* 2 n))
          (mod (* (aref *fact-inv* (+ n 1))
                  (aref *fact-inv* n))
               +mod+))
       +mod+))
