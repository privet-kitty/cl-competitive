(defpackage :cp/test/mod-polynomial
  (:use :cl :fiveam :cp/mod-polynomial)
  (:import-from :cp/test/base #:base-suite))
(in-package :cp/test/mod-polynomial)
(in-suite base-suite)

(test poly-value
  (is (= 4 (poly-value #(1 2 3) 3 30)))
  (is (= 1 (poly-value #(1 2 3) 0 30)))
  (is (= 0 (poly-value #() 0 30)))
  (is (= 0 (poly-value #() 10 30))))

(test poly-mult
  (declare (notinline poly-mult))
  (is (equalp #(2 4 6) (poly-mult #(1 2 3) #(2) 10007)))
  (is (equalp #(10006 10006 10006 3) (poly-mult #(1 2 3) #(-1 1) 10007)))
  (is (equalp #() (poly-mult #(1 2 3) #() 10007)))
  (is (equalp #() (poly-mult #() #() 10007))))

(test poly-floor!
  (declare (notinline poly-floor!))
  (multiple-value-bind (quot rem) (poly-floor! #(8 10 -5 3) #(-3 2 1) 10007)
    (is (equalp quot #(9996 3)))
    (is (equalp rem #(9982 41))))
  (let ((res #(0 0 0 0 0)))
    (multiple-value-bind (quot rem) (poly-floor! #(8 10 -5 3) #(-3 2 1) 10007 res)
      (is (equalp quot #(9996 3 0 0 0)))
      (is (eq quot res))
      (is (equalp rem #(9982 41)))))
  ;; leading zeros in dividend
  (multiple-value-bind (quot rem) (poly-floor! #(8 10 -5 3 0 0) #(-3 2 1) 10007)
    (is (equalp quot #(9996 3)))
    (is (equalp rem #(9982 41))))
  ;; leading zeros in divisor
  (multiple-value-bind (quot rem) (poly-floor! #(8 10 -5 3) #(-3 2 1 0) 10007)
    (is (equalp quot #(9996 3)))
    (is (equalp rem #(9982 41))))
  ;; leading zeros in both arguments
  (multiple-value-bind (quot rem) (poly-floor! #(8 10 -5 3 0 0) #(-3 2 1 0) 10007)
    (is (equalp quot #(9996 3)))
    (is (equalp rem #(9982 41))))
  ;; degenerative case
  (multiple-value-bind (quot rem) (poly-floor! #(8 10 -5 3 0 0) #(10 -5 3 0) 10007)
    (is (equalp quot #(0 1)))
    (is (equalp rem #(8))))
  ;; division by zero
  (signals division-by-zero (poly-floor! #(8 10 -5 3) #(0 0 0 0) 10007))
  (signals division-by-zero (poly-floor! #(8 10 -5 3) #() 10007))
  (signals division-by-zero (poly-floor! #() #() 10007))
  ;; null dividend
  (multiple-value-bind (quot rem) (poly-floor! #() #(10 -5 3 0) 10007)
    (is (equalp quot #()))
    (is (equalp rem #()))))

(test poly-mod!
  (declare (notinline poly-mod!))
  (is (equalp #(9982 41) (poly-mod! #(8 10 -5 3) #(-3 2 1) 10007)))
  ;; leading zeros in dividend
  (is (equalp #(9982 41)
              (poly-mod! #(8 10 -5 3 0 0) #(-3 2 1) 10007)))
  ;; leading zeros in divisor
  (is (equalp #(9982 41)
              (poly-mod! #(8 10 -5 3) #(-3 2 1 0) 10007)))
  ;; leading zeros in both arguments
  (is (equalp #(9982 41)
              (poly-mod! #(8 10 -5 3 0 0) #(-3 2 1 0) 10007)))
  ;; degenerative case
  (is (equalp #(8) (poly-mod! #(8 10 -5 3 0 0) #(10 -5 3 0) 10007)))
  ;; division by zero
  (signals division-by-zero (poly-mod! #(8 10 -5 3) #(0 0 0 0) 10007))
  (signals division-by-zero (poly-mod! #(8 10 -5 3) #() 10007))
  (signals division-by-zero (poly-mod! #() #() 10007))
  ;; null dividend
  (is (equalp #() (poly-mod! #() #(10 -5 3 0) 10007)))
  (is (equalp #() (poly-mod! #(0) #(10 -5 3 0) 10007))))

(test poly-differentiate!
  (declare (notinline poly-differentiate!))
  (is (equalp #(4 10 6 4) (poly-differentiate! #(3 4 5 2 1) 10007)))
  (is (equalp #(4 10 6 10003) (poly-differentiate! #(-3 4 5 2 -1) 10007)))
  (is (equalp #() (poly-differentiate! #() 10007)))
  (is (equalp #() (poly-differentiate! #(3) 10007))))

(test poly-integrate
  (is (equalp #() (poly-integrate #() 10007)))
  (is (equalp #(0 3 5004 6672 1) (poly-integrate #(3 1 2 4) 10007)))
  (is (equalp #(0 3) (poly-integrate #(3) 10007))))

(test poly-shift!
  (declare (notinline poly-shift!))
  (is (equalp #(6 1 2 4) (poly-shift! #(3 1 2 4) 3 10007)))
  (is (equalp #(3 1 2 4) (poly-shift! #(3 1 2 4) 0 10007)))
  (is (equalp #(6) (poly-shift! #(3) 3 10007)))
  (is (equalp #(3) (poly-shift! #() 3 10007)))
  (is (equalp #() (poly-shift! #(3) -3 10007)))
  (is (equalp #() (poly-shift! #() 0 10007)))
  (is (equalp #(3) (poly-shift! #(3) 0 10007))))

(test poly-scale!
  (declare (notinline poly-scale!))
  (is (equalp #(9 3 6 12) (poly-scale! #(3 1 2 4) 3 10007)))
  (is (equalp #() (poly-scale! #(3 1 2 4) 0 10007)))
  (is (equalp #() (poly-scale! #() 3 10007))))

(defconstant +mod+ 10007)
(defun make-random-polynomial (degree state)
  (let ((res (make-array degree :element-type '(unsigned-byte 31) :initial-element 0)))
    (dotimes (i degree res)
      (setf (aref res i) (random +mod+ state)))
    (let ((end (+ 1 (or (position 0 res :from-end t :test-not #'eql) -1))))
      (adjust-array res end))))

(defun poly= (p1 p2)
  (let* ((len1 (length p1))
         (len2 (length p2))
         (min-len (min len1 len2))
         (max-len (max len1 len2)))
    (loop for i below max-len
          always (cond ((< i min-len) (= (aref p1 i) (aref p2 i)))
                       ((< i len1) (zerop (aref p1 i)))
                       ((< i len2) (zerop (aref p2 i)))
                       (t (error "Huh?"))))))

(test mod-polynomial/random
  (declare (notinline poly-mult poly-div))
  (let ((state (sb-ext:seed-random-state 0)))
    (dotimes (_ 1000)
      (let ((p1 (make-random-polynomial (random 30 state) state))
            (p2 (make-random-polynomial (random 30 state) state)))
        ;; FPS division and multiplication
        (if (zerop (length p2))
            (signals division-by-zero (poly-div p1 p2 +mod+))
            (let* ((div-len (random 50))
                   (compare-len (min div-len (length p1))))
              (is (equalp (subseq p1 0 compare-len)
                          (subseq (poly-mult p2 (poly-div p1 p2 +mod+ div-len) +mod+)
                                  0 compare-len)))))
        (if (zerop (length p2))
            (signals division-by-zero (poly-div! (copy-seq p1) p2 +mod+))
            (is (equalp p1 (subseq (poly-mult p2 (poly-div! (copy-seq p1) p2 +mod+) +mod+)
                                   0 (length p1)))))))))
