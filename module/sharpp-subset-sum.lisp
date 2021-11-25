(defpackage :cp/sharpp-subset-sum
  (:use :cl :cp/binom-mod-prime :cp/fps :cp/ntt :cp/static-mod)
  (:export #:sharpp-subset-sum #:inverse-sharpp-subset-sum))
(in-package :cp/sharpp-subset-sum)

;; not tested

(declaim (ftype (function * (values ntt-vector &optional)) sharpp-subset-sum))
(defun sharpp-subset-sum (deck)
  "Converts a deck enumerator to the hand enumerator w.r.t. #P subset sum
problem:

Let D(x) and H(x) be the OGF of the sequences (d_i) and (h_i), respectively. Let
us assume that the following equality holds between D(x) and H(x): D(x) =
\prod_i (1+x^i)^(d_i). This function converts (d_i) to (h_i) in O(nlog(n))
time.

NOTE:
- h_0 = 1.
- d_0 must be 0."
  (declare (optimize (speed 3))
           (vector deck))
  (let* ((deck (coerce deck 'ntt-vector))
         (n (length deck))
         (dp (make-array n :element-type 'ntt-int :initial-element 0)))
    (declare (ntt-int n))
    (loop for i from 1 below n
          for d = (aref deck i)
          do (loop for j from 1 to (floor (- n 1) i)
                   for ij of-type (mod #.array-dimension-limit) = (* i j)
                   for incr = (* d (aref *inv* j))
                   do (setf (aref dp ij)
                            (mod (+ (aref dp ij)
                                    (if (oddp j) incr (- incr)))
                                 +mod+))))
    (poly-exp dp)))

(declaim (ftype (function * (values ntt-vector &optional))
                inverse-sharpp-subset-sum))
(defun inverse-sharpp-subset-sum (hand)
  "Converts a hand enumerator to the deck enumerator w.r.t. #P subset sum
problem:

Let D(x) and H(x) be the OGF of the sequences (d_i) and (h_i), respectively. Let
us assume that the following equality holds between D(x) and H(x): D(x) =
\prod_i (1+x^i)^(d_i). This function converts (h_i) to (d_i) in O(nlog(n))
time.

NOTE:
- h_0 must be 1.
- d_0 = 0."
  (declare (optimize (speed 3))
           (vector hand))
  (when (zerop (length hand))
    (return-from inverse-sharpp-subset-sum (make-array 0 :element-type 'ntt-int)))
  (let* ((hand (coerce hand 'ntt-vector))
         (n (length hand))
         (dp (poly-log hand)))
    (declare (ntt-int n))
    (when (> n 0)
      (assert (= (mod 1 +mod+) (aref hand 0))))
    (loop for i from 1 below n
          for h = (aref dp i)
          do (loop for j from 2 to (floor (- n 1) i)
                   for ij of-type (mod #.array-dimension-limit) = (* i j)
                   for decr = (* h (aref *inv* j))
                   do (setf (aref dp ij)
                            (mod (- (aref dp ij)
                                    (if (oddp j) decr (- decr)))
                                 +mod+))))
    dp))
