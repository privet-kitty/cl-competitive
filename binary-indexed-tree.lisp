;;;
;;; This is an implementation of binary indexed tree, specialized for the
;;; ordinary + operation. Please use the generalized-bit instead. I leave it
;;; just for my reference.
;;;

(declaim (inline bitree-inc!))
(defun bitree-inc! (bitree index delta)
  "Destructively increments the vector:
  vector[INDEX] += DELTA"
  (let ((len (length bitree)))
    (do ((i index (logior i (+ i 1))))
        ((>= i len) bitree)
      (declare ((integer 0 #.most-positive-fixnum) i))
      (setf (aref bitree i)
            (+ (aref bitree i) delta)))))

(declaim (inline bitree-sum))
(defun bitree-sum (bitree end)
  "Returns the sum: vector[0] + ... + vector[END-1]."
  (declare ((integer 0 #.most-positive-fixnum) end))
  (let ((res 0))
    (do ((i (- end 1) (- (logand i (+ i 1)) 1)))
        ((< i 0) res)
      (declare ((integer -1 #.most-positive-fixnum) i))
      (setf res (+c res (aref bitree i))))))

(defun bitree! (vector)
  "Destructively constructs BIT from VECTOR."
  (loop with len = (length vector)
        for i below len
        for dest-i = (logior i (+ i 1))
        when (< dest-i len)
        do (setf (aref vector dest-i)
                 (+ (aref vector dest-i) (aref vector i)))
        finally (return vector)))

;; Example: calculates inversion number
(declaim (inline make-reverse-lookup-table))
(defun make-reverse-lookup-table (vector &key (test #'eql))
  (let ((table (make-hash-table :test test :size (length vector))))
    (dotimes (i (length vector) table)
      (setf (gethash (aref vector i) table) i))))

(defun calc-inversion-number (vector &key (test #'<))
  (declare (vector vector))
  (let* ((len (length vector))
         (rev-lookup-table (make-reverse-lookup-table (sort (copy-seq vector) test)))
         (bitree (make-array len :element-type '(integer 0 #.most-positive-fixnum)))
         (inversion-number 0))
    (declare (integer inversion-number))
    (loop for j below len
          for element = (aref vector j)
          for compressed = (gethash element rev-lookup-table)
          for delta of-type integer = (- j (bitree-sum bitree (1+ compressed)))
          do (incf inversion-number delta)
             (bitree-inc! bitree compressed 1))
    inversion-number))

(progn
  (assert (= 3 (calc-inversion-number #(2 4 1 3 5))))
  (assert (zerop (calc-inversion-number #(0))))
  (assert (zerop (calc-inversion-number #())))
  (assert (zerop (calc-inversion-number #(1 2))))
  (assert (= 1 (calc-inversion-number #(2 1)))))
