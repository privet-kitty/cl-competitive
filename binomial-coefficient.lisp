;;;
;;; Compute binomial coefficient by direct bignum arithmetic
;;;
;;; This code ist almost the same as that of alexandria.
;;;

(declaim (inline %multiply-range))
(defun %multiply-range (i j)
  (labels ((bisect (j k)
             (declare (type (integer 1 #.most-positive-fixnum) j k)
                      (values integer))
             (if (< (- k j) 8)
                 (multiply-range j k)
                 (let ((middle (+ j (truncate (- k j) 2))))
                   (* (bisect j middle)
                      (bisect (+ middle 1) k)))))
           (multiply-range (j k)
             (declare (type (integer 1 #.most-positive-fixnum) j k))
             (do ((f k (* f m))
                  (m (1- k) (1- m)))
                 ((< m j) f)
               (declare (type (integer 0 (#.most-positive-fixnum)) m)
                        (type unsigned-byte f)))))
    (bisect i j)))

(declaim (inline factorial))
(defun factorial (n)
  (%multiply-range 1 n))

(defun binomial-coefficient (n k)
  (declare ((integer 0 (#.most-positive-fixnum)) n k))
  (assert (>= n k))
  (if (or (zerop k) (= n k))
      1
      (let ((n-k (- n k)))
        (when (< k n-k)
          (rotatef k n-k))
        (if (= 1 n-k)
            n
            (floor (%multiply-range (+ k 1) n)
	           (%multiply-range 1 n-k))))))

(defun multiset-coefficient (n k)
  (binomial-coefficient (+ n k -1) k))
