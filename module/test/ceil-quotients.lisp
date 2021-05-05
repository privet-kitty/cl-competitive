(defpackage :cp/test/ceil-quotients
  (:use :cl :fiveam :cp/ceil-quotients)
  (:import-from :cp/test/base #:base-suite))
(in-package :cp/test/ceil-quotients)
(in-suite base-suite)

(defun enum-cquotients2 (n)
  (let* ((cquot (make-cquot-manager n))
         (res (make-array (cquot-length cquot)
                          :element-type '(integer 0 #.most-positive-fixnum))))
    (dotimes (i (cquot-length cquot))
      (setf (aref res i) (cquot-get cquot i)))
    res))

(test enum-quotients
  (let ((*test-dribble* nil))
    (loop for x from 1 to 1000
          for quots2 = (enum-cquotients2 x)
          do (is (equalp (enum-cquotients x) quots2)))))
