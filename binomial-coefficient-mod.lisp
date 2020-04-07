;;;
;;; Binomial coefficient with mod
;;; build: O(n)
;;; query: O(1)
;;;

;; TODO: non-global handling

(defconstant +binom-size+ 510000)
(defconstant +binom-mod+ #.(+ (expt 10 9) 7))

(declaim ((simple-array (unsigned-byte 31) (*)) *fact* *fact-inv* *inv*))
(defparameter *fact* (make-array +binom-size+ :element-type '(unsigned-byte 31))
  "table of factorials")
(defparameter *fact-inv* (make-array +binom-size+ :element-type '(unsigned-byte 31))
  "table of inverses of factorials")
(defparameter *inv* (make-array +binom-size+ :element-type '(unsigned-byte 31))
  "table of inverses of non-negative integers")

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

(declaim (inline catalan))
(defun catalan (n)
  "Returns the N-th Catalan number."
  (declare ((integer 0 #.most-positive-fixnum) n))
  (mod (* (aref *fact* (* 2 n))
          (mod (* (aref *fact-inv* (+ n 1))
                  (aref *fact-inv* n))
               +binom-mod+))
       +binom-mod+))
