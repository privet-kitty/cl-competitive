(defpackage :cp/test/mod-sparse-polynomial
  (:use :cl :fiveam :cp/mod-sparse-polynomial :cp/mod-polynomial)
  (:import-from :cp/test/base #:base-suite))
(in-package :cp/test/mod-sparse-polynomial)
(in-suite base-suite)

(defconstant +mod+ 10007)
(defun make-random-polynomial (degree state)
  (let ((res (make-array degree :element-type '(unsigned-byte 31) :initial-element 0)))
    (dotimes (i degree res)
      (setf (aref res i) (random +mod+ state)))
    (let ((end (+ 1 (or (position 0 res :from-end t :test-not #'eql) -1))))
      (adjust-array res end))))

(defun make-random-sparse-polynomial (max-degree state)
  (loop for i to max-degree
        when (or (zerop i) (< (random 1d0 state) 0.3))
        collect (cons i (+ 1 (random (- +mod+ 1) state)))))

(defun to-dense (sparse-poly)
  (let* ((max-degree (reduce #'max sparse-poly :key #'car :initial-value 0))
         (res (make-array (+ max-degree 1) :element-type '(unsigned-byte 31) :initial-element 0)))
    (loop for (deg . coef) in sparse-poly
          do (setf (aref res deg) coef))
    res))

(test mod-polynomial/random
  (declare (notinline poly-mult poly-div))
  (let ((state (sb-ext:seed-random-state 0))
        (*test-dribble* nil))
    (dotimes (_ 1000)
      (let* ((p1 (make-random-polynomial (random 30 state) state))
             (p2 (make-random-sparse-polynomial (random 30 state) state))
             (p2-dense (to-dense p2)))
        (if (zerop (length p2))
            (signals division-by-zero (poly-sparse-div! (copy-seq p1) p2 +mod+))
            (is (equalp (poly-div! (copy-seq p1) p2-dense +mod+)
                        (poly-sparse-div! (copy-seq p1) p2 +mod+))))
        (is (equalp (subseq (poly-mult p1 p2-dense +mod+) 0 (length p1))
                    (poly-sparse-mult! p1 p2 +mod+)))))))
