(defconstant +binom-size+ 510000)
(defconstant +binom-mod+ #.(+ (expt 10 9) 7))

(declaim ((simple-array (unsigned-byte 32) (*)) *fact* *fact-inv* *inv*))
(defparameter *fact* (make-array +binom-size+ :element-type '(unsigned-byte 32)))
(defparameter *fact-inv* (make-array +binom-size+ :element-type '(unsigned-byte 32)))
(defparameter *inv* (make-array +binom-size+ :element-type '(unsigned-byte 32)))

(defun initialize-binom ()
  (setf (aref *fact* 0) 1
        (aref *fact* 1) 1
        (aref *fact-inv* 0) 1
        (aref *fact-inv* 1) 1
        (aref *inv* 1) 1)
  (loop for i from 2 below +binom-size+
        do (setf (aref *fact* i) (mod (* i (aref *fact* (- i 1))) +binom-mod+)
                 (aref *inv* i) (mod (- (* (aref *inv* (rem +binom-mod+ i))
                                           (floor +binom-mod+ i)))
                                     +binom-mod+)
                 (aref *fact-inv* i) (mod (* (aref *inv* i)
                                             (aref *fact-inv* (- i 1)))
                                          +binom-mod+))))

(initialize-binom)

(declaim (inline binom))
(defun binom (n k)
  (if (or (< n k) (< n 0) (< k 0))
      0
      (mod (* (aref *fact* n)
              (mod (* (aref *fact-inv* k) (aref *fact-inv* (- n k))) +binom-mod+))
           +binom-mod+)))
