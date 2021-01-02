(defpackage :cp/chromatic-number
  (:use :cl :cp/tzcount)
  (:export #:calc-chromatic-number)
  (:documentation "Provides O(n2^n) computation of chromatic number of
undirected graph."))
(in-package :cp/chromatic-number)

(defparameter *moduli* '(2147483647 2147483489 2147483477))
(assert (every (lambda (x) (typep x '(unsigned-byte 31)))
               *moduli*))

;; Reference: https://codeforces.com/blog/entry/57496
(defun calc-chromatic-number (mat)
  "Computes chromatic number of a given undirected graph. Time complexity is O(n2^n).

MAT := adjacency matrix

- MAT must be symmetric
- all the diagonal elements of MAT must be zero.
"
  (declare (optimize (speed 3))
           ((array * (* *)) mat))
  (let* ((n (array-dimension mat 0))
         (adjs (make-array n
                           :element-type '(integer 0 #.most-positive-fixnum)
                           :initial-element 0))
         (res n))
    (declare ((integer 0 62) n res))
    (assert (= n (array-dimension mat 1)))
    (dotimes (i n)
      (dotimes (j n)
        (when (= (aref mat i j) 1)
          (setf (ldb (byte 1 j) (aref adjs i)) 1))))
    (dolist (mod *moduli*)
      (declare ((unsigned-byte 31) mod))
      (let (;; number of independent subsets
            (ind (make-array (ash 1 n) :element-type '(unsigned-byte 31)))
            (powers (make-array (ash 1 n) ::element-type '(unsigned-byte 31))))
        ;; in advance store signs for inclusion-exclusion
        (dotimes (i (ash 1 n))
          (setf (aref powers i)
                (if (oddp (- n (logcount i)))
                    (- mod 1)
                    1)))
        (setf (aref ind 0) 1)
        (loop for i from 1 below (ash 1 n)
              for u = (tzcount i)
              do (setf (aref ind i)
                       (mod (+ (aref ind (logxor i (ash 1 u)))
                               (aref ind (logandc2 (logxor i (ash 1 u))
                                                   (aref adjs u))))
                            mod)))
        (loop for k from 1
              for sum of-type (unsigned-byte 62) = 0
              while (< k res)
              do (dotimes (i (ash 1 n))
                   (setf (aref powers i) (mod (* (aref powers i) (aref ind i))
                                              mod)
                         sum (+ sum (aref powers i))))
                 (unless (zerop (mod sum mod))
                   (setq res k)))))
    res))
