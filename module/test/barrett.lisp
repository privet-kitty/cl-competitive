(defpackage :cp/test/barrett
  (:use :cl :fiveam :cp/barrett)
  (:import-from :cp/test/base #:base-suite))
(in-package :cp/test/barrett)
(in-suite base-suite)

(defconstant +mod+ (+ (expt 10 9) 7))

(defun himod (x)
  (declare ((unsigned-byte 31) x))
  (%himod x +mod+))

(test %himod
  (is (= 0 (himod 0)))
  (is (= (- +mod+ 1) (himod (- +mod+ 1))))
  (is (= 0 (himod +mod+)))
  (is (= 1 (himod (+ +mod+ 1))))
  (is (= +mod+ (himod (* 2 +mod+)))))

(defun lomod (x)
  (declare ((signed-byte 32) x))
  (%lomod x +mod+))

(test %lomod
  (is (= 0 (lomod 0)))
  (is (= 1 (lomod 1)))
  (is (= (- +mod+ 1) (lomod (- +mod+ 1))))
  (is (= (- +mod+ 1) (lomod -1)))
  (is (= 0 (lomod (- +mod+)))))
