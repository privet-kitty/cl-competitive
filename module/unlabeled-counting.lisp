(defpackage :cp/unlabeled-counting
  (:use :cl :cp/binom-mod-prime :cp/fps :cp/ntt :cp/static-mod :cp/zeta-integer)
  (:export #:unlabeled-deck-to-hand #:unlabeled-hand-to-deck))
(in-package :cp/unlabeled-counting)

(declaim (inline %power-of-two-ceiling))
(defun %power-of-two-ceiling (x)
  (declare (ntt-int x))
  (ash 1 (integer-length (- x 1))))

(declaim (ftype (function * (values ntt-vector &optional))
                unlabeled-deck-to-hand))
(defun unlabeled-deck-to-hand (deck)
  "Converts an unlabeled deck enumerator to the (1-variable) hand
enumerator.

NOTE:
- hand[0] = 1.
- deck[0] is ignored and can be any number."
  (declare (optimize (speed 3))
           (vector deck))
  (let* ((deck (coerce deck 'ntt-vector))
         (n (length deck))
         (deck* (make-array n :element-type 'ntt-int :initial-element 0))
         (dp (make-array n :element-type 'ntt-int :initial-element 0)))
    (declare (ntt-int n))
    (dotimes (i n)
      (setf (aref deck* i) (mod (* (aref deck i) i) +mod+)))
    (divisor-transform!
     deck*
     (lambda (x y)
       (declare (ntt-int x y))
       (mod (+ x y) +mod+))
     nil)
    (when (> n 0)
      (setf (aref dp 0) (mod 1 +mod+)))
    (labels ((recur (l r)
               (declare ((mod #.array-dimension-limit) l r))
               (when (< (+ l 1) r)
                 (let ((mid (ash (+ l r) -1)))
                   (recur l mid)
                   (let* ((poly1 (subseq deck* 0 (- r l)))
                          (poly2 (subseq dp l mid))
                          (prod (poly-prod poly1 poly2)))
                     (loop for i from mid below r
                           do (setf (aref dp i)
                                    (mod (+ (aref dp i)
                                            (* (aref prod (- i l))
                                               (aref *inv* i)))
                                         +mod+))))
                   (recur mid r)))))
      (recur 0 n))
    dp))

(declaim (ftype (function * (values ntt-vector &optional))
                unlabeled-hand-to-deck))
(defun unlabeled-hand-to-deck (hand)
  "Converts an unlabeld hand enumerator to the deck enumerator.

NOTE:
- hand[0] must be 1.
- deck[0] = 0."
  (declare (optimize (speed 3))
           (vector hand))
  (let* ((hand (coerce hand 'ntt-vector))
         (n (length hand))
         (dp (make-array n :element-type 'ntt-int :initial-element 0)))
    (declare (ntt-int n))
    (when (> n 0)
      (assert (= (mod 1 +mod+) (aref hand 0))))
    (dotimes (i n)
      (setf (aref dp i) (mod (* (aref hand i) i) +mod+)))
    (labels ((recur (l r)
               (declare ((mod #.array-dimension-limit) l r))
               (when (< (+ l 1) r)
                 (let ((mid (ash (+ l r) -1)))
                   (recur l mid)
                   (let* ((poly1 (subseq hand 0 (- r l)))
                          (poly2 (subseq dp l mid))
                          (prod (poly-prod poly1 poly2)))
                     (loop for i from mid below r
                           do (setf (aref dp i)
                                    (mod (- (aref dp i)
                                            (aref prod (- i l)))
                                         +mod+))))
                   (recur mid r)))))
      (recur 0 n))
    (inverse-divisor-transform!
     dp
     (lambda (x y)
       (declare (ntt-int x y))
       (mod (- x y) +mod+))
     nil)
    (loop for i from 1 below n
          do (setf (aref dp i)
                   (mod (* (aref dp i) (aref *inv* i)) +mod+)))
    dp))
