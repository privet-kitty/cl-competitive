;;;
;;; Complex FFT
;;;
;;; Reference:
;;; http://www.prefield.com/algorithm/math/fft.html
;;; http://techtipshoge.blogspot.com/2011/08/fft4.html (Japanese)
;;;

(deftype fft-float () 'double-float)

(declaim (inline power2-p))
(defun power2-p (x)
  "Checks if X is a power of 2."
  (zerop (logand x (- x 1))))

(defun %dft! (f sign)
  (declare (optimize (speed 3) (safety 0))
           ((simple-array (complex fft-float) (*)) f)
           ((integer -1 1) sign))
  (prog1 f
    (let* ((n (length f))
           (theta (* sign (/ (coerce (* 2 pi) 'fft-float) n))))
      (declare (fft-float theta))
      (assert (power2-p n))
      (do ((m n (ash m -1)))
          ((= m 1))
        (declare ((integer 0 #.most-positive-fixnum) m))
        (let ((mh (ash m -1)))
          (declare ((integer 0 #.most-positive-fixnum) mh))
          (dotimes (i mh)
            (let ((w (cis (* i theta))))
              (do ((j i (+ j m)))
                  ((>= j n))
                (declare ((integer 0 #.most-positive-fixnum) j))
                (let* ((k (+ j mh))
                       (xt (- (aref f j) (aref f k))))
                  (declare ((integer 0 #.most-positive-fixnum) k))
                  (incf (aref f j) (aref f k))
                  (setf (aref f k) (* w xt))))))
          (setq theta (* theta 2))))
      ;; bit-reverse ordering
      (let ((i 0))
        (declare ((integer 0 #.most-positive-fixnum) i))
        (loop for j from 1 below (- n 1)
              do (loop for k of-type (integer 0 #.most-positive-fixnum)
                          = (ash n -1) then (ash k -1)
                       while (> k (setq i (logxor i k))))
                 (when (< j i)
                   (rotatef (aref f i) (aref f j))))))))

;; For FFT of fixed length, preparing the table of exp(i*theta) will be
;; efficient.
(defun %make-exp-table (n sign)
  (declare (optimize (speed 3))
           ((integer 0 #.most-positive-fixnum) n)
           ((integer -1 1) sign))
  (assert (power2-p n))
  (let* ((table (make-array (ash n -1) :element-type '(complex fft-float)))
         (theta (* sign (/ (coerce (* 2 pi) 'fft-float) n))))
    (dotimes (i (length table))
      (setf (aref table i) (cis (* i theta))))
    table))

(defparameter *exp-table+* nil)
(defparameter *exp-table-* nil)

(defmacro with-fixed-length-fft (size &body body)
  "Makes FFT faster when the size of target vectors is fixed in BODY. This macro
computes and holds the roots of unity for SIZE, which DFT! and INVERSE-DFT!
called in BODY automatically uses; they will signal an error when they receive a
vector of different size."
  (let ((s (gensym)))
    `(let ((,s ,size))
       (let ((*exp-table+* (%make-exp-table ,s 1))
             (*exp-table-* (%make-exp-table ,s -1)))
         ,@body))))

(defun %dft-cached-cis! (f sign)
  (declare (optimize (speed 3) (safety 0))
           ((simple-array (complex fft-float) (*)) f)
           ((integer -1 1) sign))
  (prog1 f
    (let ((n (length f))
          (table (if (= 1 sign) *exp-table+* *exp-table-*)))
      (declare ((simple-array (complex fft-float) (*)) table))
      (assert (power2-p n))
      (assert (>= (length table) (ash n -1)))
      (do ((m n (ash m -1))
           (shift 0 (+ shift 1)))
          ((= m 1))
        (declare ((integer 0 #.most-positive-fixnum) m shift))
        (let ((mh (ash m -1)))
          (dotimes (i mh)
            (do ((j i (+ j m)))
                ((>= j n))
              (declare ((integer 0 #.most-positive-fixnum) j))
              (let* ((k (+ j mh))
                     (xt (- (aref f j) (aref f k)))
                     (cis-index (ash i shift)))
                (declare ((integer 0 #.most-positive-fixnum) k cis-index))
                (incf (aref f j) (aref f k))
                (setf (aref f k) (* (aref table cis-index) xt)))))))
      ;; bit-reverse ordering
      (let ((i 0))
        (declare ((integer 0 #.most-positive-fixnum) i))
        (loop for j from 1 below (- n 1)
              do (loop for k of-type (integer 0 #.most-positive-fixnum)
                          = (ash n -1) then (ash k -1)
                       while (> k (setq i (logxor i k))))
                 (when (< j i)
                   (rotatef (aref f i) (aref f j))))))))

(declaim (inline dft!))
(defun dft! (vector)
  "Does DFT on VECTOR. The length of VECTOR must be a power of 2."
  (declare ((simple-array (complex fft-float) (*)) vector))
  (if (zerop (length vector))
      vector
      (if *exp-table+*
          (%dft-cached-cis! vector 1)
          (%dft! vector 1))))

(declaim (inline inverse-dft!))
(defun inverse-dft! (vector)
  "Does inverse DFT on VECTOR. The length of VECTOR must be a power of 2."
  (declare ((simple-array (complex fft-float) (*)) vector))
  (prog1 vector
    (let ((n (length vector)))
      (unless (zerop n)
        (let ((/n (/ (coerce n 'fft-float))))
          (if *exp-table-*
              (%dft-cached-cis! vector -1)
              (%dft! vector -1))
          (dotimes (i n)
            (setf (aref vector i) (* (aref vector i) /n))))))))

(declaim (inline convolute!))
(defun convolute! (vector1 vector2 &optional result-vector)
  "Returns the convolution of two vectors VECTOR1 and VECTOR2. A new vector is
created when RESULT-VECTOR is null. This function destructively modifies VECTOR1
and VECTOR2. (They can be restored by INVERSE-DFT!.)"
  (declare ((simple-array (complex fft-float) (*)) vector1 vector2)
           ((or null (simple-array (complex fft-float) (*))) result-vector))
  (let ((n (length vector1)))
    (assert (and (power2-p n)
                 (= n (length vector2))))
    (dft! vector1)
    (dft! vector2)
    (let ((result (or result-vector
                      (make-array n :element-type '(complex fft-float)))))
      (dotimes (i n)
        (setf (aref result i) (* (aref vector1 i) (aref vector2 i))))
      (inverse-dft! result))))
