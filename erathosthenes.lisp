(declaim (ftype (function * (values simple-bit-vector &optional)) make-prime-table))
(defun make-prime-table (size)
  "Erzeugt die Primzahlentabelle für 0, 1, ...,  SIZE-1."
  (declare (optimize (speed 3) (safety 0)))
  (check-type size (mod #.array-total-size-limit))
  (let ((dict (make-array size :element-type 'bit :initial-element 1)))
    (setf (sbit dict 0) 0
          (sbit dict 1) 0)
    (loop for even-num from 4 below size by 2
          do (setf (sbit dict even-num) 0))
    (loop for p from 3 to (+ 1 (isqrt (- size 1))) by 2
          when (= 1 (sbit dict p))
          do (loop for composite from (* 2 p) below size by p
                   until (>= composite size)
                   do (setf (sbit dict composite) 0)))
    dict))

(declaim (inline decompose-to-pow-table))
(defun decompose-to-pow-table (num prime-table)
  (declare (optimize (speed 3))
           ((integer 1) num)
           (simple-bit-vector prime-table))
  (let ((factor-table (make-array (length prime-table)
                                  :element-type '(unsigned-byte 32)
                                  :initial-element 0)))
    (when (> (length prime-table) 2)
      (setf (aref factor-table 2)
            (loop while (evenp num)
                  count t
                  do (setf num (ash num -1)))))
    (loop for prime from 3 to (min num (- (length prime-table) 1)) by 2
          when (= 1 (sbit prime-table prime))
          do (setf (aref factor-table prime)
                   (loop with quot and rem
                         do (setf (values quot rem) (floor num prime))
                         while (zerop rem)
                         count t
                         do (setf num quot)))
          finally (return factor-table))))


(declaim (ftype (function * (values list &optional)) factorize))
(defun factorize (x prime-table)
  "Returns the associative list of prime factors of X, which is composed
of (<prime> . <exponent>). E.g. (factorize 100 <prime-table>) => '((2 . 2) (5
. 5)).

Note that the returned list is NOT guaranteed to be in ascending order."
  (declare ((integer 1) x)
           (simple-bit-vector prime-table))
  (assert (>= (length prime-table) 3))
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
                                 (return (cons prime exponent))))))))
         when factor-cons
         collect factor-cons into res
         finally (return
                   (if (= x 1)
                       res
                       (cons (cons x 1) res))))))
