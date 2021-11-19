(defpackage :cp/polynomial-ntt
  (:use :cl :cp/ntt :cp/mod-inverse :cp/mod-power :cp/mod-sqrt :cp/static-mod)
  (:export #:poly-prod #:poly-inverse #:poly-floor #:poly-mod #:poly-sub #:poly-add
           #:multipoint-eval #:poly-total-prod #:chirp-z #:bostan-mori
           #:poly-differentiate! #:poly-integrate
           #:poly-log #:poly-exp #:poly-power #:poly-sqrt))
(in-package :cp/polynomial-ntt)

;; TODO: integrate with cp/polynomial

(define-ntt +mod+
  :convolve poly-prod)

(declaim (inline %adjust))
(defun %adjust (vector size)
  (declare (ntt-vector vector))
  (if (or (null size) (= size (length vector)))
      vector
      (let ((res (make-array size :element-type 'ntt-int :initial-element 0)))
        (replace res vector)
        res)))

(declaim (inline %power-of-two-ceiling))
(defun %power-of-two-ceiling (x)
  (declare (ntt-int x))
  (ash 1 (integer-length (- x 1))))

;; (declaim (ftype (function * (values ntt-vector &optional)) poly-inverse))
;; (defun poly-inverse (poly &optional result-length)
;;   (declare (optimize (speed 3))
;;            (vector poly)
;;            ((or null fixnum) result-length))
;;   (let* ((poly (coerce poly 'ntt-vector))
;;          (n (length poly)))
;;     (declare (ntt-vector poly))
;;     (when (or (zerop n)
;;               (zerop (aref poly 0)))
;;       (error 'division-by-zero
;;              :operation #'poly-inverse
;;              :operands (list poly)))
;;     (let ((res (make-array 1
;;                            :element-type 'ntt-int
;;                            :initial-element (mod-inverse (aref poly 0) +mod+)))
;;           (result-length (or result-length n)))
;;       (declare (ntt-vector res))
;;       (loop for i of-type ntt-int = 1 then (ash i 1)
;;             while (< i result-length)
;;             for decr = (poly-prod (poly-prod res res)
;;                                   (subseq poly 0 (min (length poly) (* 2 i))))
;;             for decr-len = (length decr)
;;             do (setq res (%adjust res (* 2 i) :initial-element 0))
;;                (dotimes (j (* 2 i))
;;                  (setf (aref res j)
;;                        (mod (the ntt-int
;;                                  (+ (mod (* 2 (aref res j)) +mod+)
;;                                     (if (>= j decr-len) 0 (- +mod+ (aref decr j)))))
;;                             +mod+))))
;;       (%adjust res result-length))))

;; Reference: https://opt-cp.com/fps-fast-algorithms/
(declaim (ftype (function * (values ntt-vector &optional)) poly-inverse))
(defun poly-inverse (poly &optional result-length)
  (declare (optimize (speed 3))
           (vector poly)
           ((or null fixnum) result-length))
  (let* ((poly (coerce poly 'ntt-vector))
         (n (length poly)))
    (declare (ntt-vector poly))
    (when (or (zerop n)
              (zerop (aref poly 0)))
      (error 'division-by-zero
             :operation #'poly-inverse
             :operands (list poly)))
    (let* ((result-length (or result-length n))
           (res (make-array 1
                            :element-type 'ntt-int
                            :initial-element (mod-inverse (aref poly 0) +mod+))))
      (declare (ntt-vector res))
      (loop for i of-type ntt-int = 1 then (ash i 1)
            while (< i result-length)
            for f of-type ntt-vector = (make-array (* 2 i) :element-type 'ntt-int
                                                           :initial-element 0)
            for g of-type ntt-vector = (make-array (* 2 i) :element-type 'ntt-int
                                                           :initial-element 0)
            do (replace f poly :end2 (min n (* 2 i)))
               (replace g res)
               (ntt! f)
               (ntt! g)
               (dotimes (j (* 2 i))
                 (setf (aref f j) (mod (* (aref g j) (aref f j)) +mod+)))
               (inverse-ntt! f)
               (replace f f :start1 0 :end1 i :start2 i :end2 (* 2 i))
               (fill f 0 :start i :end (* 2 i))
               (ntt! f)
               (dotimes (j (* 2 i))
                 (setf (aref f j) (mod (* (aref g j) (aref f j)) +mod+)))
               (inverse-ntt! f)
               (let ((inv-len (mod-inverse (* 2 i) +mod+)))
                 (setq inv-len (mod (* inv-len (- +mod+ inv-len))
                                    +mod+))
                 (dotimes (j i)
                   (setf (aref f j) (mod (* inv-len (aref f j)) +mod+)))
                 (setq res (%adjust res (* 2 i)))
                 (replace res f :start1 i)))
      (%adjust res result-length))))

(declaim (ftype (function * (values ntt-vector &optional)) poly-floor))
(defun poly-floor (poly1 poly2)
  (declare (optimize (speed 3))
           (vector poly1 poly2))
  (let* ((poly1 (coerce poly1 'ntt-vector))
         (poly2 (coerce poly2 'ntt-vector))
         (len1 (+ 1 (or (position 0 poly1 :from-end t :test-not #'eql) -1)))
         (len2 (+ 1 (or (position 0 poly2 :from-end t :test-not #'eql) -1))))
    (when (> len2 len1)
      (return-from poly-floor (make-array 0 :element-type 'ntt-int)))
    (setq poly1 (nreverse (subseq poly1 0 len1))
          poly2 (nreverse (subseq poly2 0 len2)))
    (let* ((res-len (+ 1 (- len1 len2)))
           (res (%adjust (poly-prod poly1 (poly-inverse poly2 res-len))
                         res-len)))
      (nreverse res))))

(declaim (ftype (function * (values ntt-vector &optional)) poly-sub))
(defun poly-sub (poly1 poly2)
  (declare (optimize (speed 3))
           (vector poly1 poly2))
  (let* ((poly1 (coerce poly1 'ntt-vector))
         (poly2 (coerce poly2 'ntt-vector))
         (len (max (length poly1) (length poly2)))
         (res (make-array len :element-type 'ntt-int :initial-element 0)))
    (replace res poly1)
    (dotimes (i (length poly2))
      (let ((value (- (aref res i) (aref poly2 i))))
        (setf (aref res i)
              (if (< value 0)
                  (+ value +mod+)
                  value))))
    (let ((end (+ 1 (or (position 0 res :from-end t :test-not #'eql) -1))))
      (%adjust res end))))

(declaim (ftype (function * (values ntt-vector &optional)) poly-add))
(defun poly-add (poly1 poly2)
  (declare (optimize (speed 3))
           (vector poly1 poly2))
  (let* ((poly1 (coerce poly1 'ntt-vector))
         (poly2 (coerce poly2 'ntt-vector))
         (len (max (length poly1) (length poly2)))
         (res (make-array len :element-type 'ntt-int :initial-element 0)))
    (replace res poly1)
    (dotimes (i (length poly2))
      (setf (aref res i) (mod (+ (aref res i) (aref poly2 i)) +mod+)))
    (let ((end (+ 1 (or (position 0 res :from-end t :test-not #'eql) -1))))
      (%adjust res end))))

(declaim (ftype (function * (values ntt-vector &optional)) poly-mod))
(defun poly-mod (poly1 poly2)
  (declare (optimize (speed 3))
           (vector poly1 poly2))
  (let ((poly1 (coerce poly1 'ntt-vector))
        (poly2 (coerce poly2 'ntt-vector)))
    (when (loop for x across poly1 always (zerop x))
      (return-from poly-mod (make-array 0 :element-type 'ntt-int)))
    (let* ((res (poly-sub poly1 (poly-prod (poly-floor poly1 poly2) poly2)))
           (end (+ 1 (or (position 0 res :from-end t :test-not #'eql) -1))))
      (subseq res 0 end))))

(declaim (ftype (function * (values ntt-vector &optional)) poly-total-prod))
(defun poly-total-prod (polys)
  "Returns the total polynomial product: polys[0] * polys[1] * ... * polys[n-1]."
  (declare (vector polys))
  (let* ((n (length polys))
         (dp (make-array n :element-type t)))
    (declare ((mod #.array-dimension-limit) n))
    (when (zerop n)
      (return-from poly-total-prod (make-array 1 :element-type 'ntt-int :initial-element 1)))
    (replace dp polys)
    (loop for width of-type (mod #.array-dimension-limit) = 1 then (ash width 1)
          while (< width n)
          do (loop for i of-type (mod #.array-dimension-limit) from 0 by (* width 2)
                   while (< (+ i width) n)
                   do (setf (aref dp i)
                            (poly-prod (aref dp i) (aref dp (+ i width))))))
    (coerce (the vector (aref dp 0)) 'ntt-vector)))

(declaim (ftype (function * (values ntt-vector &optional)) multipoint-eval))
(defun multipoint-eval (poly points)
  "The length of POINTS must be a power of two."
  (declare (optimize (speed 3))
           (vector poly points)
           #+sbcl (sb-ext:muffle-conditions style-warning))
  (check-ntt-vector points)
  (let* ((poly (coerce poly 'ntt-vector))
         (points (coerce points 'ntt-vector))
         (len (length points))
         (table (make-array (max 0 (- (* 2 len) 1)) :element-type 'ntt-vector))
         (res (make-array len :element-type 'ntt-int)))
    (unless (zerop len)
      (labels ((%build (l r pos)
                 (declare ((mod #.array-dimension-limit) l r pos))
                 (if (= (- r l) 1)
                     (let ((lin (make-array 2 :element-type 'ntt-int)))
                       (setf (aref lin 0) (- +mod+ (aref points l)) ;; NOTE: non-zero
                             (aref lin 1) 1)
                       (setf (aref table pos) lin))
                     (let ((mid (ash (+ l r) -1)))
                       (%build l mid (+ 1 (* pos 2)))
                       (%build mid r (+ 2 (* pos 2)))
                       (setf (aref table pos)
                             (poly-prod (aref table (+ 1 (* pos 2)))
                                        (aref table (+ 2 (* pos 2)))))))))
        (%build 0 len 0))
      (labels ((%eval (poly l r pos)
                 (declare ((mod #.array-dimension-limit) l r pos))
                 (if (= (- r l) 1)
                     (let ((tmp (poly-mod poly (aref table pos))))
                       (setf (aref res l) (if (zerop (length tmp)) 0 (aref tmp 0))))
                     (let ((mid (ash (+ l r) -1)))
                       (%eval (poly-mod poly (aref table (+ (* 2 pos) 1)))
                              l mid (+ (* 2 pos) 1))
                       (%eval (poly-mod poly (aref table (+ (* 2 pos) 2)))
                              mid r (+ (* 2 pos) 2))))))
        (%eval poly 0 len 0)))
    res))

;; not tested
(declaim (ftype (function * (values ntt-vector &optional)) chirp-z))
(defun chirp-z (poly base length)
  "Does multipoint evaluation of POLY with powers of BASE: P(base^0), P(base^1), ...,
P(base^(length-1)). BASE must be coprime with modulus. Time complexity is
O((N+MOD)log(N+MOD)).

Reference:
https://codeforces.com/blog/entry/83532"
  (declare (optimize (speed 3))
           (vector poly)
           (ntt-int base length))
  (when (zerop (length poly))
    (return-from chirp-z (make-array length :element-type 'ntt-int :initial-element 0)))
  (let* ((poly (coerce poly 'ntt-vector))
         (binv (mod-inverse base +mod+))
         (n (length poly))
         (m (max length n))
         (n+m (+ n m))
         (cs (make-array n :element-type 'ntt-int :initial-element 0))
         (ds (make-array n+m :element-type 'ntt-int :initial-element 0)))
    (declare (ntt-int n m n+m))
    (dotimes (i n)
      (setf (aref cs i) (mod (* (aref poly (- n 1 i))
                                (mod-power binv (ash (* (- n 1 i) (- n 2 i)) -1) +mod+))
                             +mod+)))
    (dotimes (i n+m)
      (setf (aref ds i) (mod-power base (ash (* i (- i 1)) -1) +mod+)))
    (let ((result (subseq (poly-prod cs ds)
                          (- n 1)
                          (+ (- n 1) length))))
      (dotimes (i length)
        (setf (aref result i)
              (mod (* (aref result i)
                      (mod-power binv (ash (* i (- i 1)) -1) +mod+))
                   +mod+)))
      result)))

(declaim (ftype (function * (values ntt-int &optional)) bostan-mori))
(defun bostan-mori (index num denom)
  "Returns [x^index](num(x)/denom(x)).

Reference:
https://arxiv.org/abs/2008.08822
https://qiita.com/ryuhe1/items/da5acbcce4ac1911f47 (Japanese)"
  (declare (optimize (speed 3))
           (unsigned-byte index)
           (vector num denom))
  (labels ((even (p)
             (let ((res (make-array (ceiling (length p) 2) :element-type 'ntt-int)))
               (dotimes (i (length res))
                 (setf (aref res i)
                       (aref p (* 2 i))))
               res))
           (odd (p)
             (let ((res (make-array (floor (length p) 2) :element-type 'ntt-int)))
               (dotimes (i (length res))
                 (setf (aref res i) (aref p (+ 1 (* 2 i)))))
               res))
           (negate (p)
             (let ((res (copy-seq p)))
               (loop for i from 1 below (length res) by 2
                     do (setf (aref res i)
                              (if (zerop (aref res i))
                                  0
                                  (- +mod+ (aref res i)))))
               res)))
    (let ((num (coerce num 'ntt-vector))
          (denom (coerce denom 'ntt-vector)))
      (when (or (zerop (length denom))
                (zerop (aref denom 0)))
        (error 'division-by-zero
               :operands (list num denom)
               :operation 'bostan-mori))
      (loop while (> index 0)
            for denom- = (negate denom)
            for u = (poly-prod num denom-)
            when (evenp index)
            do (setq num (even u))
            else
            do (setq num (odd u))
            do (setq denom (even (poly-prod denom denom-))
                     index (ash index -1))
            finally (return (rem (* (if (zerop (length num))
                                        0
                                        (aref num 0))
                                    (mod-inverse (aref denom 0) +mod+))
                                 +mod+))))))

(declaim (inline poly-differentiate!))
(defun poly-differentiate! (p)
  "Returns the derivative of P."
  (declare (vector p))
  (let ((p (coerce p 'ntt-vector)))
    (when (zerop (length p))
      (return-from poly-differentiate! p))
    (dotimes (i (- (length p) 1))
      (declare (ntt-int i))
      (setf (aref p i)
            (mod (* (aref p (+ i 1)) (+ i 1)) +mod+)))
    (let ((end (+ 1 (or (position 0 p :from-end t :end (- (length p) 1) :test-not #'eql)
                        -1))))
      (subseq p 0 end))))

(declaim (ntt-vector *inv*))
(defparameter *inv* (make-array 2 :element-type 'ntt-int :initial-contents '(0 1)))

(defun fill-inv! (new-size)
  (declare (optimize (speed 3))
           ((mod #.array-dimension-limit) new-size))
  (let* ((old-size (length *inv*))
         (new-size (%power-of-two-ceiling (max old-size new-size))))
    (when (< old-size new-size)
      (loop with inv of-type ntt-vector = (%adjust *inv* new-size)
            for x from old-size below new-size
            do (setf (aref inv x)
                     (- +mod+
                        (mod (* (aref inv (rem +mod+ x)) (floor +mod+ x))
                             +mod+)))
            finally (setq *inv* inv)))))

(declaim (inline poly-integrate))
(defun poly-integrate (p)
  "Returns an indefinite integral of P. Assumes the integration constant to
be zero."
  (declare (vector p))
  (let* ((p (coerce p 'ntt-vector))
         (n (length p)))
    (when (zerop n)
      (return-from poly-integrate (make-array 0 :element-type 'ntt-int)))
    (fill-inv! (+ n 1))
    (let ((result (make-array (+ n 1) :element-type 'ntt-int :initial-element 0))
          (inv *inv*))
      (dotimes (i n)
        (setf (aref result (+ i 1))
              (mod (* (aref p i) (aref inv (+ i 1)))
                   +mod+)))
      result)))

(declaim (ftype (function * (values ntt-vector &optional)) poly-log))
(defun poly-log (poly &optional result-length)
  (declare (optimize (speed 3))
           (vector poly)
           ((or null (integer 1 (#.array-dimension-limit))) result-length))
  (let* ((poly (coerce poly 'ntt-vector))
         (result-length (or result-length (length poly))))
    (assert (= 1 (aref poly 0)))
    (let ((res (poly-integrate (%adjust (poly-prod (poly-differentiate! (copy-seq poly))
                                                   (poly-inverse poly result-length))
                                        (- result-length 1)))))
      (%adjust res result-length))))

(declaim (ftype (function * (values ntt-vector &optional)) poly-exp))
(defun poly-exp (poly &optional result-length)
  (declare (optimize (speed 3))
           (vector poly)
           ((or null (mod #.array-dimension-limit)) result-length))
  (assert (or (zerop (length poly)) (zerop (aref poly 0))))
  (let ((poly (coerce poly 'ntt-vector))
        (result-length (or result-length (length poly)))
        (res (make-array 1 :element-type 'ntt-int :initial-element 1)))
    (loop until (>= (length res) result-length)
          for new-len of-type (mod #.array-dimension-limit) = (* 2 (length res))
          for log = (poly-log res new-len)
          do (loop for i from 0 below (min new-len (length poly))
                   do (setf (aref log i)
                            (mod (- (aref poly i) (aref log i))
                                 +mod+)))
             (setf (aref log 0) (mod (+ 1 (aref log 0)) +mod+)
                   res (%adjust (poly-prod res log) new-len)))
    (%adjust res result-length)))

(declaim (ftype (function * (values ntt-vector &optional)) poly-power))
(defun poly-power (poly exp &optional result-length)
  (declare (optimize (speed 3))
           (vector poly)
           ((integer 0 #.most-positive-fixnum) exp)
           ((or null (mod #.array-dimension-limit)) result-length))
  (let* ((poly (coerce poly 'ntt-vector))
         (result-length (or result-length (length poly)))
         (init-pos (position 0 poly :test-not #'eql)))
    (when (or (null init-pos)
              (zerop result-length)
              (> (* init-pos exp) result-length))
      (return-from poly-power
        (make-array result-length :element-type 'ntt-int :initial-element 0)))
    (let ((tmp (subseq poly init-pos)))
      (let ((inv (mod-inverse (aref poly init-pos) +mod+)))
        (dotimes (i (length tmp))
          (setf (aref tmp i) (mod (* (aref tmp i) inv) +mod+))))
      (setq tmp (poly-log tmp result-length))
      (let ((exp (mod exp +mod+)))
        (dotimes (i (length tmp))
          (setf (aref tmp i) (mod (* (aref tmp i) exp) +mod+)))
        (setq tmp (poly-exp tmp result-length)))
      (let ((power (mod-power (aref poly init-pos) exp +mod+)))
        (dotimes (i (length tmp))
          (setf (aref tmp i) (mod (* (aref tmp i) power) +mod+))))
      (if (zerop init-pos)
          tmp
          (let ((res (make-array result-length :element-type 'ntt-int :initial-element 0)))
            (replace res tmp :start1 (min (length res) (* init-pos exp)))
            res)))))

(defconstant +/2+ (ash (+ +mod+ 1) -1))

(declaim (ftype (function * (values (or null ntt-vector) &optional)) poly-sqrt))
(defun poly-sqrt (poly &optional result-length)
  (declare (optimize (speed 3))
           (vector poly)
           ((or null (mod #.array-dimension-limit)) result-length))
  (let* ((result-length (or result-length (length poly)))
         (poly (coerce poly 'ntt-vector)))
    (labels ((return-zero ()
               (return-from poly-sqrt
                 (make-array result-length :element-type 'ntt-int :initial-element 0))))
      (when (zerop (length poly))
        (return-zero))
      (if (zerop (aref poly 0))
          (let ((i (position 0 poly :test-not #'=)))
            (unless i
              (return-zero))
            (when (oddp i)
              (return-from poly-sqrt))
            (when (<= result-length (ash i -1))
              (return-zero))
            (let ((tmp-res (poly-sqrt (subseq poly i) (- result-length (ash i -1)))))
              (unless tmp-res
                (return-from poly-sqrt))
              (let ((res (make-array result-length :element-type 'ntt-int :initial-element 0)))
                (replace res tmp-res :start1 (ash i -1))
                res)))
          (let ((sqrt (mod-sqrt (aref poly 0) +mod+)))
            (unless sqrt
              (return-from poly-sqrt))
            (assert (= (aref poly 0) (mod (* sqrt sqrt) +mod+)))
            (let ((res (make-array 1 :element-type 'ntt-int :initial-element sqrt)))
              (loop for len of-type (mod #.array-dimension-limit) = 1 then (ash len 1)
                    while (< len result-length)
                    do (let* ((next-len (ash len 1))
                              (tmp (poly-prod (poly-inverse res next-len)
                                              (%adjust poly next-len))))
                         (declare ((mod #.array-dimension-limit) next-len))
                         (setq res (%adjust res next-len))
                         (dotimes (i next-len)
                           (setf (aref res i)
                                 (mod (* +/2+ (mod (+ (aref res i) (aref tmp i)) +mod+))
                                      +mod+)))))
              (%adjust res result-length)))))))
