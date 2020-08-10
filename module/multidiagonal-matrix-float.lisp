;;;
;;; Several operations on a multidiagonal matrix (double-float)
;;;

(defstruct (multidiagonal-matrix
            (:constructor make-md-matrix
                (size k
                 &aux
                 (storage (make-array (list size (+ 1 (* 2 k)))
                                      :element-type 'double-float
                                      :initial-element 0d0))))
            (:conc-name md-))
  (size 0 :type (integer 0 #.most-positive-fixnum))
  (k 0 :type (integer 0 #.most-positive-fixnum))
  (storage nil :type (simple-array double-float (* *))))

(declaim (inline md-gemv))
(defun md-gemv (md-matrix vector)
  (let* ((storage (md-storage md-matrix))
         (size (length vector))
         (k (md-k md-matrix))
         (result (make-array size
                             :element-type 'double-float
                             :initial-element 0d0)))
    (assert (= size (md-size md-matrix)))
    (dotimes (i size)
      (let ((value 0d0))
        (declare (double-float value))
        ;; VECTOR[i] corresponds to STORAGE[][K]
        (loop for j from (max 0 (- i k)) to (min (- size 1) (+ i k))
              do (incf value
                       (* (aref vector j)
                          (aref storage i (+ k (- j i))))))
        (setf (aref result i) value)))
    result))

(declaim (inline md-gemv!))
(defun md-gemv! (md-matrix vector result-vector)
  (let* ((storage (md-storage md-matrix))
         (size (length vector))
         (k (md-k md-matrix)))
    (assert (= size
               (md-size md-matrix)
               (length result-vector)))
    (dotimes (i size)
      (let ((value 0d0))
        (declare (double-float value))
        ;; VECTOR[i] corresponds to STORAGE[][K]
        (loop for j from (max 0 (- i k)) to (min (- size 1) (+ i k))
              do (incf value
                       (* (aref vector j)
                          (aref storage i (+ k (- j i))))))
        (setf (aref result-vector i) value)))
    result-vector))

(declaim (inline md-ref))
(defun md-ref (md-matrix i j)
  (let* ((storage (md-storage md-matrix))
         (k (md-k md-matrix))
         (internal-j (+ k (- j i))))
    (aref storage i internal-j)))

(declaim (inline (setf md-ref)))
(defun (setf md-ref) (new-value md-matrix i j)
  (declare ((integer 0 #.most-positive-fixnum) i j))
  (let* ((storage (md-storage md-matrix))
         (k (md-k md-matrix))
         (internal-j (+ k (- j i))))
    (setf (aref storage i internal-j) new-value)))
