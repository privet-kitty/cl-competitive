(defpackage :cp/mod-log
  (:use :cl :cp/mod-inverse)
  (:export #:mod-log))
(in-package :cp/mod-log)

(declaim (ftype (function * (values (or null (integer 0 #.most-positive-fixnum)) &optional)) mod-log))
(defun mod-log (x y modulus &key from-zero)
  "Returns the smallest positive integer k that satiefies x^k ≡ y mod p.
Returns NIL if it is infeasible."
  (declare (optimize (speed 3))
           (integer x y)
           ((integer 1 #.most-positive-fixnum) modulus))
  (let ((x (mod x modulus))
        (y (mod y modulus))
        (g (gcd x modulus)))
    (declare (optimize (safety 0))
             ((mod #.most-positive-fixnum) x y g))
    (when (and from-zero (or (= y 1) (= modulus 1)))
      (return-from mod-log 0))
    (if (= g 1)
        ;; coprime case
        (let* ((m (+ 1 (isqrt (- modulus 1)))) ; smallest integer equal to or
                                               ; larger than sqrt(p)
               (x^m (loop for i below m
                          for res of-type (integer 0 #.most-positive-fixnum) = x
                          then (mod (* res x) modulus)
                          finally (return res)))
               (table (make-hash-table :size m :test 'eq)))
          ;; Constructs TABLE: yx^j |-> j (j = 0, ..., m-1)
          (loop for j from 0 below m
                for res of-type (integer 0 #.most-positive-fixnum) = y
                then (mod (* res x) modulus)
                do (setf (gethash res table) j))
          ;; Finds i and j that satisfy (x^m)^i = yx^j and returns m*i-j
          (loop for i from 1 to m
                for x^m^i of-type (integer 0 #.most-positive-fixnum) = x^m
                then (mod (* x^m^i x^m) modulus)
                for j = (gethash x^m^i table)
                when j
                do (locally
                       (declare ((integer 0 #.most-positive-fixnum) j))
                     (return (- (* i m) j)))
                finally (return nil)))
        ;; If x and p are not coprime, let g := gcd(x, p), x := gx', y := gy', p
        ;; := gp' and solve x^(k-1) ≡ y'x'^(-1) mod p' instead. See
        ;; https://math.stackexchange.com/questions/131127/ for the detail.
        (if (= x y)
            ;; This is tha special treatment for the case x ≡ y. Without this
            ;; (mod-log 4 0 4) returns not 1 but 2.
            1
            (multiple-value-bind (y-prime rem) (floor y g)
              (if (zerop rem)
                  (let* ((x-prime (floor x g))
                         (p-prime (floor modulus g))
                         (next-rhs (mod (* y-prime (mod-inverse x-prime p-prime)) p-prime))
                         (res (mod-log x next-rhs p-prime)))
                    (declare ((integer 0 #.most-positive-fixnum) x-prime p-prime next-rhs))
                    (if res (+ 1 res) nil))
                  nil))))))
