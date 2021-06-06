(defpackage :cp/test/floor-quotients
  (:use :cl :fiveam :cp/floor-quotients)
  (:import-from :cp/test/base #:base-suite))
(in-package :cp/test/floor-quotients)
(in-suite base-suite)

(defun %identity (vec n)
  (nreverse (map 'vector (lambda (x) (+ 1 (floor n x))) vec)))

(test enum-fdivisors
  (let ((*test-dribble* nil))
    (is (equalp #(1 2) (enum-fdivisors 1)))
    (is (equalp #(1 2 3) (enum-fdivisors 2)))
    (is (equalp #(1 2 3 4 6 11) (enum-fdivisors 10)))
    (loop for x from 0 to 1000
          for vec = (enum-fdivisors x)
          do (is (equalp (%identity vec x) vec)))))

(defun enum-fdivisors2 (n)
  (let* ((fquot (make-fquot-manager n))
         (res (make-array (+ 1 (fquot-length fquot))
                          :element-type '(integer 0 #.most-positive-fixnum))))
    (dotimes (i (+ 1 (fquot-length fquot)))
      (setf (aref res i) (fquot-get fquot i)))
    res))

(test enum-fdivisors2
  (let ((*test-dribble* nil))
    (loop for x from 0 to 1000
          for quots2 = (enum-fdivisors2 x)
          for fquot = (make-fquot-manager x)
          do (is (equalp (enum-fdivisors x) quots2))
             (dotimes (i (length quots2))
               (is (= i (fquot-index fquot (aref quots2 i))))))))
