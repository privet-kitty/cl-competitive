(defpackage :cp/test/polynomial-ntt
  (:use :cl :fiveam :cp/ntt :cp/polynomial-ntt)
  (:import-from :cp/test/base #:base-suite)
  (:import-from :cp/polynomial #:poly-value))
(in-package :cp/test/polynomial-ntt)
(in-suite base-suite)

(test polynomial-ntt/manual
  (signals division-by-zero (poly-inverse #()))
  (signals division-by-zero (poly-inverse #(0 2))))

(defun make-random-polynomial (degree)
  (let ((res (make-array degree :element-type 'ntt-int :initial-element 0)))
    (dotimes (i degree res)
      (setf (aref res i) (random +ntt-mod+)))
    (let ((end (+ 1 (or (position 0 res :from-end t :test-not #'eql) -1))))
      (adjust-array res end))))

(test polynomial-ntt/random
  (finishes
    (dotimes (_ 1000)
      (let* ((len1 (random 10))
             (len2 (random 10))
             (poly1 (make-random-polynomial len1))
             (poly2 (make-random-polynomial len2)))
        ;; inverse
        (when (find-if #'plusp poly1)
          (let ((res (ntt-convolute poly1 (poly-inverse poly1))))
            (assert (= 1 (aref res 0)))
            (loop for i from 1 below len1
                  do (assert (zerop (aref res i))))))
        ;; floor and mod
        (block continue
          (handler-bind ((division-by-zero (lambda (c) (declare (ignorable c))
                                             (return-from continue))))
            (let* ((p (poly-floor poly1 poly2))
                   (q (poly-sub poly1 p)))
              (equalp q (poly-mod poly1 poly2)))))
        ;; multipoint eval.
        (let* ((points (make-random-polynomial (ash 1 (random 7))))
               (res1 (map 'ntt-vector (lambda (x) (poly-value poly1 x +ntt-mod+)) points))
               (res2 (multipoint-eval poly1 points)))
          (assert (equalp res1 res2)))))))
