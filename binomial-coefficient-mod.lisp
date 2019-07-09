;;;
;;; Binomial coefficient with mod
;;; build: O(n)
;;; query: O(1)
;;;

(defconstant +binom-size+ 510000)
(defconstant +binom-mod+ #.(+ (expt 10 9) 7))

(declaim ((simple-array (unsigned-byte 32) (*)) *fact* *fact-inv* *inv*))
(defparameter *fact* (make-array +binom-size+ :element-type '(unsigned-byte 32)))
(defparameter *fact-inv* (make-array +binom-size+ :element-type '(unsigned-byte 32)))
(defparameter *inv* (make-array +binom-size+ :element-type '(unsigned-byte 32)))

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

(defun multinomial (&rest ks)
  "Returns the multinomial coefficient K!/k_1!k_2!...k_n! for K = k_1 + k_2 +
... + k_n. K must be equal or smaller than MOST-POSITIVE-FIXNUM. (multinomial)
returns 1."
  (let ((sum 0)
        (result 1))
    (declare ((integer 0 #.most-positive-fixnum) result sum))
    (dolist (k ks)
      (incf sum k)
      (setq result
            (mod (* result (aref *fact-inv* k)) +binom-mod+)))
    (mod (* result (aref *fact* sum)) +binom-mod+)))
