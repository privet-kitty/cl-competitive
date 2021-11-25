(defpackage :cp/unlabeled-counting
  (:use :cl :cp/binom-mod-prime :cp/fps :cp/ntt :cp/static-mod)
  (:export #:unlabeled-deck-to-hand #:unlabeled-hand-to-deck))
(in-package :cp/unlabeled-counting)

(declaim (ftype (function * (values ntt-vector &optional))
                unlabeled-deck-to-hand))
(defun unlabeled-deck-to-hand (deck)
  "Converts an unlabeled deck enumerator to the hand enumerator:

Let D(x) and H(x) be the OGF of the sequences (d_i) and (h_i), respectively. Let
us assume that the following equality holds between D(x) and H(x): D(x) =
\prod_i (1-x^i)^(-d_i). This function converts (d_i) to (h_i) in O(nlog(n))
time.

NOTE:
- hand[0] = 1.
- deck[0] is ignored and can be any number."
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
                   do (setf (aref dp ij)
                            (mod (+ (* d (aref *inv* j)) (aref dp ij))
                                 +mod+))))
    (poly-exp dp)))

(declaim (ftype (function * (values ntt-vector &optional))
                unlabeled-hand-to-deck))
(defun unlabeled-hand-to-deck (hand)
  "Converts an unlabeled hand enumerator to the deck enumerator:

Let D(x) and H(x) be the OGF of the sequences (d_i) and (h_i), respectively. Let
us assume that the following equality holds between D(x) and H(x): D(x) =
\prod_i (1-x^i)^(-d_i). This function converts (h_i) to (d_i) in O(nlog(n))
time.

NOTE:
- hand[0] must be 1.
- deck[0] = 0."
  (declare (optimize (speed 3))
           (vector hand))
  (when (zerop (length hand))
    (return-from unlabeled-hand-to-deck (make-array 0 :element-type 'ntt-int)))
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
                   do (setf (aref dp ij)
                            (mod (- (aref dp ij) (* h (aref *inv* j)))
                                 +mod+))))
    dp))
