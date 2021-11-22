(defpackage :cp/unlabeled-counting
  (:use :cl :cp/binom-mod-prime :cp/fps :cp/ntt :cp/static-mod)
  (:export #:unlabeled-deck-to-hand))
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
- deck[0] is ignored."
  (declare (optimize (speed 3))
           (vector deck))
  (let* ((deck (coerce deck 'ntt-vector))
         (n (length deck))
         (deck* (make-array n :element-type 'ntt-int :initial-element 0))
         (dp (make-array n :element-type 'ntt-int :initial-element 0)))
    (declare (ntt-int n))
    (loop for r from 1 below n
          do (loop for x from r below n by r
                   do (setf (aref deck* x)
                            (mod (+ (aref deck* x) (* (aref deck r) r))
                                 +mod+))))
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
