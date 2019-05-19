(declaim (ftype (function * (values simple-bit-vector &optional)) make-prime-table))
(defun make-prime-table (sup)
  "Erzeugt die Primzahlentabelle für 0, 1, ...,  SUP-1."
  (declare (optimize (speed 3) (safety 0)))
  (check-type sup (integer 2 (#.array-total-size-limit)))
  (let ((dict (make-array sup :element-type 'bit :initial-element 1)))
    (setf (sbit dict 0) 0
          (sbit dict 1) 0)
    (loop for even-num from 4 below sup by 2
          do (setf (sbit dict even-num) 0))
    (loop for p from 3 to (+ 1 (isqrt (- sup 1))) by 2
          when (= 1 (sbit dict p))
          do (loop for composite from (* 2 p) below sup by p
                   until (>= composite sup)
                   do (setf (sbit dict composite) 0)))
    dict))

(defun make-prime-sequence (sup)
  "Returns the ascending sequence of primes smaller than SUP."
  (declare (optimize (speed 3) (safety 0)))
  (check-type sup (integer 2 (#.array-total-size-limit)))
  (let ((dict (make-array sup :element-type 'bit :initial-element 1)))
    (setf (sbit dict 0) 0
          (sbit dict 1) 0)
    (loop for even-num from 4 below sup by 2
          do (setf (sbit dict even-num) 0))
    (loop for p from 3 to (+ 1 (isqrt (- sup 1))) by 2
          when (= 1 (sbit dict p))
          do (loop for composite from (* 2 p) below sup by p
                   until (>= composite sup)
                   do (setf (sbit dict composite) 0)))
    (let* ((length (count 1 dict))
           (result (make-array length :element-type '(integer 0 #.most-positive-fixnum)))
           (index 0))
      (declare ((integer 0 #.most-positive-fixnum) length))
      (loop for x below sup
            when (= 1 (sbit dict x))
            do (setf (aref result index) x)
               (incf index))
      result)))

(declaim (ftype (function * (values list &optional)) factorize-with-table))
(defun factorize-with-table (x prime-table)
  "Returns the associative list of prime factors of X, which is composed
of (<prime> . <exponent>). E.g. (factorize 100 <prime-table>) => '((2 . 2) (5
. 5)).

PRIME-TABLE := simple-bit-vector (PRIME-TABLE[k] = 1 iff k is prime)

Note that the returned list is NOT guaranteed to be in ascending order."
  (declare (integer x)
           (simple-bit-vector prime-table))
  (assert (>= (length prime-table) 3))
  (setq x (abs x))
  (if (zerop x)
      nil
      (append
       (loop for exponent of-type (integer 0 #.most-positive-fixnum) from 0
             while (evenp x)
             do (setf x (ash x -1))
             finally (return
                       (when (> exponent 0)
                         (list (cons 2 exponent)))))
       (loop for prime from 3 to (min x (- (length prime-table) 1)) by 2
             for factor-cons =
                (when (= 1 (sbit prime-table prime))
                  (loop for exponent of-type (integer 0 #.most-positive-fixnum) from 0
                        do (multiple-value-bind (quot rem) (floor x prime)
                             (if (zerop rem)
                                 (setf x quot)
                                 (return
                                   (when (> exponent 0)
                                     (cons prime exponent)))))))
             when factor-cons
             collect factor-cons into res
             finally (return
                       (if (= x 1)
                           res
                           (cons (cons x 1) res)))))))

(declaim (ftype (function * (values list &optional)) factorize))
(defun factorize (x prime-set)
  "Returns the associative list of prime factors of X, which is composed
of (<prime> . <exponent>). E.g. (factorize 100 <prime-table>) => '((2 . 2) (5
. 5)).

PRIME-SET := sequence (composed only of not duplicated primes)
Note that the returned list is NOT guaranteed to be in ascending order."
  (declare (integer x)
           (sequence prime-set))
  (setq x (abs x))
  (if (zerop x)
      nil
      (let (result)
        (labels ((process (prime)
                   (declare ((integer 0 #.most-positive-fixnum) prime))
                   (when (= x 1)
                     (return-from factorize result))
                   (loop for exponent of-type (integer 0 #.most-positive-fixnum) from 0
                         do (multiple-value-bind (quot rem) (floor x prime)
                              (if (zerop rem)
                                  (setf x quot)
                                  (return
                                    (when (> exponent 0)
                                      (push (cons prime exponent) result))))))))
          (declare (inline process)
                   (dynamic-extent #'process))
          (map () #'process prime-set))
        (if (= x 1)
            result
            (cons (cons x 1) result)))))
