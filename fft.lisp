;;;
;;; In-place FFT
;;;
;;; Reference:
;;; http://www.prefield.com/algorithm/math/fft.html
;;; http://techtipshoge.blogspot.com/2011/08/fft4.html
;;;

(deftype fft-float () 'double-float)

(declaim (inline power2-p))
(defun power2-p (x)
  "Checks if X is a power of 2."
  (zerop (logand x (- x 1))))

(defun %make-cis-table (n sign)
  (declare ((integer 0 #.most-positive-fixnum) n)
           ((integer -1 1) sign))
  (assert (power2-p n))
  (let* ((table (make-array (ash n -1) :element-type '(complex fft-float)))
         (theta (* sign (/ (coerce (* 2 pi) 'fft-float) n))))
    (dotimes (i (length table))
      (setf (aref table i) (cis (* i theta))))
    table))

(defun %general-dft! (f sign)
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
      (let ((i 0))
        (declare ((integer 0 #.most-positive-fixnum) i))
        (loop for j from 1 below (- n 1)
              do (loop for k of-type (integer 0 #.most-positive-fixnum)
                          = (ash n -1) then (ash k -1)
                       while (> k (setq i (logxor i k))))
                 (when (< j i)
                   (rotatef (aref f i) (aref f j))))))))

(declaim (inline dft!))
(defun dft! (f)
  (declare ((simple-array (complex fft-float) (*)) f))
  (if (zerop (length f))
      f
      (%general-dft! f 1)))

(declaim (inline inverse-dft!))
(defun inverse-dft! (f)
  (declare ((simple-array (complex fft-float) (*)) f))
  (prog1 f
    (let ((n (length f)))
      (unless (zerop n)
        (let ((/n (/ (coerce n 'fft-float))))
          (%general-dft! f -1)
          (dotimes (i n)
            (setf (aref f i) (* (aref f i) /n))))))))

(declaim (inline convolute!))
(defun convolute! (g h)
  (declare ((simple-array (complex fft-float) (*)) g h))
  (assert (and (power2-p (length g))
               (power2-p (length h))
               (= (length g) (length h))))
  (let ((n (length g)))
    (dft! g)
    (dft! h)
    (let ((f (make-array n :element-type '(complex fft-float))))
      (dotimes (i n)
        (setf (aref f i) (* (aref g i) (aref h i))))
      (inverse-dft! f))))
