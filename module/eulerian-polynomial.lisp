(defpackage :cp/eulerian-polynomial
  (:use :cl
   :cp/ntt :cp/polynomial-ntt :cp/perfect-kth-powers :cp/binom-mod-prime :cp/static-mod)
  (:export #:make-eulerian-polynomial))
(in-package :cp/eulerian-polynomial)

(defun make-eulerian-polynomial (n minfactor-table)
  "Returns a sequence A(n, 0), A(n, 1), ..., A(n, n-1), where A(n, m) is the
number of permutations of the length n which contains exactly m descent (or
ascent)."
  (declare (optimize (speed 3))
           ((mod #.array-dimension-limit) n)
           (vector minfactor-table))
  (let* ((len (ceiling n 2))
         (poly1 (make-array len :element-type 'ntt-int :initial-element 0))
         (poly2 (subseq (make-perfect-kth-powers minfactor-table (+ len 1) n +mod+) 1)))
    (dotimes (k len)
      (let ((val (mod (* (aref *fact-inv* k)
                         (aref *fact-inv* (- (+ n 1) k)))
                      +mod+)))
        (setf (aref poly1 k)
              (if (evenp k) val (mod (- val) +mod+)))))
    (let ((res (adjust-array (poly-multiply poly1 poly2) n))
          (coef (aref *fact* (+ n 1))))
      (declare (ntt-vector res))
      (dotimes (i len)
        (setf (aref res i) (mod (* (aref res i) coef) +mod+)))
      (loop for i from len below n
            do (setf (aref res i) (aref res (- n i 1))))
      res)))
