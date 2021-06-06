(defpackage :cp/floor-quotients
  (:use :cl)
  (:export #:make-fquot-manager #:fquot-length #:fquot-n #:fquot-short-p #:fquot-fsqrt
           #:fquot-index #:fquot-get #:enum-fdivisors #:map-fdivisors))
(in-package :cp/floor-quotients)

;; TODO: more tests and docs

(defstruct (fquot-manager (:constructor %make-fquot-manager)
                          (:conc-name fquot-))
  (n nil :type (integer 0 #.most-positive-fixnum))
  (length nil :type (integer 0 #.most-positive-fixnum))
  (short-p nil :type boolean)
  (fsqrt nil :type (integer 0 #.most-positive-fixnum)))

(defun make-fquot-manager (n)
  (declare (optimize (speed 3))
           ((integer 0 #.most-positive-fixnum) n))
  (let* ((fsqrt (isqrt n))
         (short-p (and (> n 0) (= fsqrt (floor n fsqrt))))
         (length (- (* 2 fsqrt) (if short-p 1 0))))
    (%make-fquot-manager :n n :length length :short-p short-p :fsqrt fsqrt)))

(declaim (inline fquot-index))
(defun fquot-index (fquot x)
  "Returns the position of x on the sequence of representive divisors."
  (declare ((integer 1 #.most-positive-fixnum) x))
  (if (<= x (fquot-fsqrt fquot))
      (- x 1)
      (- (fquot-length fquot)
         (floor (fquot-n fquot) x))))

(declaim (inline fquot-get))
(defun fquot-get (fquot i)
  "Returns the i-th element of the sequence of representative divisors."
  (declare ((integer 0 #.most-positive-fixnum) i))
  (let ((length (fquot-length fquot)))
    (assert (<= i length))
    (if (< i (fquot-fsqrt fquot))
        (+ i 1)
        (let ((rev-i (- length i)))
          (+ 1 (floor (fquot-n fquot) (+ rev-i 1)))))))

(declaim (inline %calc-length))
(defun %calc-length (x)
  (let ((sqrt (isqrt x)))
    (if (and (> x 0) (= (floor x sqrt) sqrt))
        (* 2 sqrt)
        (+ 1 (* 2 sqrt)))))

(declaim (ftype (function * (values (simple-array (integer 0 #.most-positive-fixnum) (*))
                                    &optional))
                enum-fdivisors))
(defun enum-fdivisors (n)
  "Given a positive integer N, floor(N/k) takes at most O(sqrt(N)) values for k
in {1, ..., N}. ENUM-FDIVISORS returns the ascending vector of minimal integers
k that take `new' values compared to floor(N/(k-1)). Note that this vector
contains 1 and N+1.

Returns #(1) for convenience when N == 0.

Tips: Let A be the returned vector of length M+1 and an index i in {0, ..., M}
be given; then A[i]A[j] <= N iff j < M-i."
  (declare (optimize (speed 3))
           ((integer 0 #.most-positive-fixnum) n))
  (let ((k 1)
        (result (make-array (%calc-length n)
                            :element-type '(integer 0 #.most-positive-fixnum)))
        (end 0))
    (declare ((mod #.array-dimension-limit) end))
    (loop (setf (aref result end) k)
          (when (> k n)
            (return result))
          (incf end)
          (setq k (+ 1 (floor n (floor n k)))))))

;; Another implementation:
;; (defun enum-fdivisros2 (n)
;;   (let ((sqrt (isqrt n))
;;         (res (make-array 0 :element-type 'fixnum :fill-pointer 0)))
;;     (loop for i from 1 below sqrt
;;           do (vector-push-extend i res))
;;     (vector-push-extend sqrt res)
;;     (unless (= sqrt (floor n sqrt))
;;       (vector-push-extend (+ 1 (floor n (+ sqrt 1))) res))
;;     (loop for i from (- sqrt 1) downto 1
;;           do (vector-push-extend (+ 1 (floor n (+ i 1))) res))
;;     res))

(declaim (inline map-fdivisors))
(defun map-fdivisors (function n)
  (declare ((integer 0 #.most-positive-fixnum) n))
  (let ((k 1))
    (loop (funcall function k)
          (when (> k n)
            (return))
          (setq k (+ 1 (floor n (floor n k)))))))
