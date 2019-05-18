(declaim ((simple-array double-float (*)) *bernoulli*))
(defparameter *bernoulli*
  #.(coerce
     (mapcar (lambda (x) (float x 1d0))
             '(1 -1/2 1/6 0 -1/30 0 1/42 0 -1/30 0 5/66 0 -691/2730 0 7/6 0 -3617/510 0 43867/798 0 -174611/330 0 854513/138 0 -236364091/2730 0 8553103/6 0 -23749461029/870 0 8615841276005/14332 0))
     '(simple-array double-float (*))))

(declaim ((simple-array (unsigned-byte 62) (*)) *factorial-table*))
(defparameter *factorial-table*
  (make-array 21 :element-type '(unsigned-byte 62) :initial-contents '(1 1 2 6 24 120 720 5040 40320 362880 3628800 39916800 479001600 6227020800 87178291200 1307674368000 20922789888000 355687428096000 6402373705728000 121645100408832000 2432902008176640000)))

;; FIXME: Maybe we should rely on a convergent series instead of the asymptotic
;; expansion. See
;; https://en.wikipedia.org/wiki/Stirling%27s_approximation#A_convergent_version_of_Stirling's_formula
(declaim (ftype (function * (values double-float &optional)) log-factorial))
(defun log-factorial (n &optional (terms 4))
  "Returns log(n!). Note that the returned value is just an approximation."
  (declare (optimize (speed 3))
           ((integer 0) n)
           ((unsigned-byte 8) terms))
  (if (<= n 20)
      (log (float (aref *factorial-table* n) 1d0))
      (let ((n (float n 1d0)))
        (+ #.(log (sqrt (* 2d0 pi)))
           (* (+ n 0.5d0) (log n))
           (- n)
           (loop for i2 from 2 to (* terms 2) by 2
                 sum (/ (aref *bernoulli* i2)
                        (* i2 (- i2 1) (expt n (- i2 1))))
                 of-type double-float)))))

(defun log-binomial (n k &optional (terms 2))
  (declare ((integer 0) n k))
  (assert (>= n k))
  (- (log-factorial n terms)
     (log-factorial (- n k) terms)
     (log-factorial k terms)))
