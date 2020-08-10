;; TODO: enable to deal with a given arithmetric progression

;; not tested
(defstruct (stair-sum (:constructor %make-stair-sum))
  (cumul nil :type (simple-array fixnum (*)))
  (stair nil :type (simple-array fixnum (*))))

(declaim (inline make-stair-sum))
(defun make-stair-sum (vector)
  "Makes a table from VECTOR that stores 0, VECTOR[0], VECTOR[0] + 2*VECTOR[1],
VECTOR[0] + 2*VECTOR[1] + 3*VECTOR[2], ..."
  (declare (vector vector))
  (let* ((n (length vector))
         (cumul (make-array (+ n 1) :element-type 'fixnum :initial-element 0))
         (stair (make-array (+ n 1) :element-type 'fixnum :initial-element 0)))
    (dotimes (i n)
      (setf (aref stair (+ i 1))
            (+ (aref stair i) (* (+ i 1) (the fixnum (aref vector i))))
            (aref cumul (+ i 1))
            (+ (aref cumul i) (the fixnum (aref vector i)))))
    (%make-stair-sum :cumul cumul :stair stair)))

(declaim (inline stair-sum-query))
(defun stair-sum-query (stair-sum l r)
  "Returns VECTOR[L] + 2*VECTOR[L+1] + 3*VECTOR[L+2] + ... + (R-L)*VECTOR[R-1]."
  (symbol-macrolet ((stair (stair-sum-stair stair-sum))
                    (cumul (stair-sum-cumul stair-sum)))
    (- (- (aref stair r)
          (aref stair l))
       (* l (- (aref cumul r)
               (aref cumul l))))))
