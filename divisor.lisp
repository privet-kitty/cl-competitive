(declaim (ftype (function * (values (vector (integer 0 #.most-positive-fixnum)) &optional))
                enum-divisors))
(defun enum-divisors (x)
  "Enumerates all the divisors of X in O(sqrt(X)). Note that the resultant
vector is NOT sorted."
  (declare (optimize (speed 3))
           ((integer 0 #.most-positive-fixnum) x))
  (let* ((sqrt (isqrt x))
         (res (make-array (isqrt sqrt) ; FIXME: sets the initial size to x^1/4
                          :element-type '(integer 0 #.most-positive-fixnum)
                          :fill-pointer 0)))
    (loop for i from 1 to sqrt
          do (multiple-value-bind (quot rem) (floor x i)
               (when (zerop rem)
                 (vector-push-extend i res)
                 (unless (= i quot)
                   (vector-push-extend quot res)))))
    res))

;; Below is the variant that returns a sorted list.
;; (defun enum-divisors (n)
;;   "Returns the increasing list of all the divisors of N."
;;   (declare (optimize (speed 3))
;;            ((integer 1 #.most-positive-fixnum) n))
;;   (if (= n 1)
;;       (list 1)
;;       (let* ((sqrt (isqrt n))
;;              (result (list 1)))
;;         (labels ((%enum (i first-half second-half)
;;                    (declare ((integer 1 #.most-positive-fixnum) i))
;;                    (cond ((or (< i sqrt)
;;                               (and (= i sqrt) (/= (* sqrt sqrt) n)))
;;                           (multiple-value-bind (quot rem) (floor n i)
;;                             (if (zerop rem)
;;                                 (progn
;;                                   (setf (cdr first-half) (list i))
;;                                   (setf second-half (cons quot second-half))
;;                                   (%enum (1+ i) (cdr first-half) second-half))
;;                                 (%enum (1+ i) first-half second-half))))
;;                          ((= i sqrt) ; N is a square number here
;;                           (setf (cdr first-half) (cons i second-half)))
;;                          (t ; (> i sqrt)
;;                           (setf (cdr first-half) second-half)))))
;;           (%enum 2 result (list n))
;;           result))))

(defun make-divisors-table (sup)
  "Returns the vector whose each cell, vector[INDEX], is the increasing list of
every divisor of INDEX lower than SUP. Note that vector[0] = NIL."
  (declare ((integer 0 #.most-positive-fixnum) sup))
  (let ((result (make-array sup :element-type 'list))
        (tails (make-array sup :element-type 'list)))
    (declare (optimize (speed 3) (safety 0)))
    (labels ((push-back (number x)
               (setf (cdr (aref tails number)) (list x))
               (setf (aref tails number) (cdr (aref tails number)))))
      (declare (inline push-back))
      (loop for i from 1 below sup
            for cell = (list 1)
            do (setf (aref result i) cell
                     (aref tails i) cell))
      (when (>= sup 1)
        (setf (aref result 0) nil))
      (loop for divisor from 2 below sup
            do (loop for number from divisor below sup by divisor
                     do (push-back number divisor)))
      result)))
