(defpackage :cp/test/suffix-array
  (:use :cl :fiveam :cp/suffix-array)
  (:import-from :cp/test/base #:base-suite)
  (:import-from :cp/suffix-array #:%sa-is))
(in-package :cp/test/suffix-array)
(in-suite base-suite)

(defun %sa-naive (vector)
  (declare (optimize (speed 3))
           (sa-vector vector)
           (inline sort sb-impl::stable-sort-list))
  (let* ((n (length vector))
         (sa (make-array n :element-type 'sa-int :initial-element 0)))
    (dotimes (i n)
      (setf (aref sa i) i))
    (sort sa (lambda (l r)
               (declare ((mod #.array-dimension-limit) l r))
               (and (/= l r)
                    (loop for i from l below n
                          for j from r below n
                          unless (= (aref vector i) (aref vector j))
                          do (return (< (aref vector i) (aref vector j)))
                          finally (return (= i n))))))))

(test suffix-array/random
  (dotimes (len 20)
    (finishes
      (let ((s (make-array len :element-type 'sa-int :initial-element 0)))
        (dolist (alphabet-size '(1 2 3 5 8 8 10 10 20 50))
          (dotimes (_ 50)
            (dotimes (i len)
              (setf (aref s i) (random alphabet-size)))
            (assert (equalp (%sa-naive s) (%sa-is s alphabet-size)))))))))

(test suffix-array/hand
  (is (equalp (vector) (make-suffix-array "" :order #'char<)))
  (is (equalp #(0) (make-suffix-array "3" :key #'char-code)))
  (is (equalp #(15 14 10 6 2 11 7 3 1 0 13 12 9 5 8 4)
              (make-suffix-array "mmiissiissiippii" :order #'char<)))
  (is (equalp #(15 14 10 6 2 11 7 3 1 0 13 12 9 5 8 4)
              (make-suffix-array "mmiissiissiippii" :key #'char-code)))
  (is (equalp #(15 14 10 6 2 11 7 3 1 0 13 12 9 5 8 4)
              (make-suffix-array "mmiissiissiippii" :key #'char-code))))
