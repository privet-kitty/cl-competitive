(defpackage :cp/test/mod-linear-algebra
  (:use :cl :fiveam :cp/mod-linear-algebra)
  (:import-from :cp/test/base #:base-suite))
(in-package :cp/test/mod-linear-algebra)
(in-suite base-suite)

(defconstant +mod+ 1000000007)

(test mod-echelon
  (declare (notinline mod-echelon!))
  (is (equalp #2a((1 0 1000000005 1000000004) (0 1 1 4) (0 0 0 0))
              (mod-echelon! (make-array '(3 4) :initial-contents '((1 3 1 9) (1 1 -1 1) (3 11 5 35))) +mod+)))
  (is (= 2 (nth-value 1 (mod-echelon! (make-array '(3 4) :initial-contents '((1 3 1 9) (1 1 -1 1) (3 11 5 35))) +mod+))))
  (is (equalp #2a((1 0 1000000005 0) (0 1 1 0) (0 0 0 1))
              (mod-echelon! (make-array '(3 4) :initial-contents '((1 3 1 9) (1 1 -1 1) (3 11 5 37))) +mod+)))
  (is (= 3 (nth-value 1 (mod-echelon! #2a((1 3 1 9) (1 1 -1 1) (3 11 5 37)) +mod+))))
  ;; extended
  (is (equalp #2a((1 0 1000000005 1000000004) (0 1 1 4) (0 0 0 1))
              (mod-echelon! (make-array '(3 4) :initial-contents '((1 3 1 9) (1 1 -1 1) (3 11 5 36))) +mod+ t)))
  (is (= 2 (nth-value 1 (mod-echelon! (make-array '(3 4) :initial-contents '((1 3 1 9) (1 1 -1 1) (3 11 5 36))) +mod+ t))))
  (is (equalp #2a((1 0 0 4) (0 1 0 3) (0 0 1 0))
              (mod-echelon! (make-array '(3 4) :initial-contents '((3 1 4 1) (5 2 6 5) (0 5 2 1))) 7 t))))

(test mod-determinant
  (declare (notinline mod-determinant!))
  (is (= 14 (mod-determinant! (make-array '(4 4) :initial-contents '((3 3 3 1) (2 4 5 2) (3 4 5 1) (2 2 3 4))) +mod+)))
  (is (= (mod -70 +mod+)
         (mod-determinant! (make-array '(3 3) :initial-contents '((10 20 10) (4 5 6) (2 3 5))) +mod+)))
  (is (= 1 (mod-determinant! (make-array '(0 0)) +mod+))))

(test mod-inverse-matrix
  (declare (notinline mod-inverse-matrix!))
  (is (equalp #2a((1 0 0) (0 1 0) (0 0 1))
              (mod-inverse-matrix! (make-array '(3 3) :initial-contents '((1 0 0) (0 1 0) (0 0 1))) 7)))
  (is (equalp #2a((0 0 1) (0 1 0) (1 0 0))
              (mod-inverse-matrix! (make-array '(3 3) :initial-contents '((0 0 1) (0 1 0) (1 0 0))) 7)))
  (is (null (mod-inverse-matrix! (make-array '(3 3) :initial-contents '((0 0 1) (1 1 1) (1 1 1))) 7))))
