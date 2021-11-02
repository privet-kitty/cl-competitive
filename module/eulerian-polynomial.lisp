(defpackage :cp/eulerian-polynomial
  (:use :cl
   :cp/ntt :cp/polynomial-ntt :cp/perfect-kth-powers :cp/binom-mod-prime :cp/static-mod)
  (:export #:make-eulerian-polynomial #:make-eulerian-polynomial*))
(in-package :cp/eulerian-polynomial)

(defun make-eulerian-polynomial (n minfactor-table)
  "Returns a sequence A(n, 0), A(n, 1), ..., A(n, n-1), where A(n, m) is the
number of permutations of the length n which contains exactly m descent (or
ascent). Note that the 0-th eulerian polynomial is 1 by definition."
  (declare (optimize (speed 3))
           ((mod #.array-dimension-limit) n)
           (vector minfactor-table))
  (when (zerop n)
    (return-from make-eulerian-polynomial
      (make-array 1 :element-type 'ntt-int :initial-element 1)))
  (let* ((len (ceiling n 2))
         (poly1 (make-array len :element-type 'ntt-int :initial-element 0))
         (poly2 (subseq (make-perfect-kth-powers minfactor-table (+ len 1) n +mod+) 1)))
    (dotimes (k len)
      (let ((val (mod (* (aref *fact-inv* k)
                         (aref *fact-inv* (- (+ n 1) k)))
                      +mod+)))
        (setf (aref poly1 k)
              (if (evenp k) val (mod (- val) +mod+)))))
    (let ((res (adjust-array (poly-prod poly1 poly2) n))
          (coef (aref *fact* (+ n 1))))
      (declare (ntt-vector res))
      (dotimes (i len)
        (setf (aref res i) (mod (* (aref res i) coef) +mod+)))
      (loop for i from len below n
            do (setf (aref res i) (aref res (- n i 1))))
      res)))

(defun make-eulerian-polynomial* (n minfactor-table)
  "Returns a sequence A'(n, 0), A'(n, 1), ..., A'(n, n), where A'(n, m) is the
number of permutations of the length n which contains exactly m runs.

Note: Run is a maximal contiguous increasing subsequence. It holds A'(n, m) =
A'(n, m-1) for almost all args. This variant will be more useful when we deal
with FPS of n-th powers and assume 0^0 = 1."
  (declare (optimize (speed 3))
           ((mod #.array-dimension-limit) n)
           (vector minfactor-table))
  (if (= n 0)
      (make-array 1 :element-type 'ntt-int :initial-element 1)
      (concatenate '(simple-array ntt-int (*))
                   #(0)
                   (make-eulerian-polynomial n minfactor-table))))
