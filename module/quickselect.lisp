(defpackage :cp/quickselect
  (:use :cl)
  (:export #:quickselect!)
  (:documentation
   "Provides selection algorithm, i.e., an algorithm to find the k-th least (or
greatest) element in a vector. Time complexity is expected O(Nlog(N))."))
(in-package :cp/quickselect)

(declaim (inline %hoare-partition!))
(defun %hoare-partition! (vector l r pivot-idx order)
  "Destructively rearranges VECTOR[L, R] and returns an index i: All the
elements less than VECTOR[PIVOT-IDX] are moved to the front of i and those
larger behind i. Note that this function deals with the **closed** interval [L,
R]."
  (declare (vector vector)
           ((mod #.array-dimension-limit) l r))
  (rotatef (aref vector pivot-idx)
           (aref vector l))
  (let ((pivot (aref vector l))
        (i l)
        (j (+ r 1)))
    (declare (fixnum i j))
    (loop
      (loop do (incf i)
            while (and (<= i r)
                       (funcall order (aref vector i) pivot)))
      (loop do (decf j)
            while (funcall order pivot (aref vector j)))
      (when (>= i j)
        (rotatef (aref vector l) (aref vector j))
        (return j))
      (rotatef (aref vector i) (aref vector j)))))

;; FIXME: When we use Lomuto partition scheme for QUICKSELECT!, it takes O(n^2)
;; time for a vector consisting of a single value like #(1 1 1 1 1).
(defun %lomuto-partition! (vector l r pivot-idx order)
  (rotatef (aref vector r) (aref vector pivot-idx))
  (let ((pivot (aref vector r))
        (base l))
    (loop for j from l below r
          unless (funcall order pivot (aref vector j))
          do (rotatef (aref vector base) (aref vector j))
             (incf base))
    (rotatef (aref vector base) (aref vector r))
    base))

(declaim (inline quickselect!))
(defun quickselect! (vector order i &optional (start 0) end)
  "Returns the (0-based) i-th least element of VECTOR (if order is #'<, for
example). VECTOR is destructively modified as follows: VECTOR[START],
VECTOR[START+1], ..., VECTOR[START+i] are equal to or less than VECTOR[START+i],
and VECTOR[START+i], VECTOR[START+i+1], ..., VECTOR[END-1] are equal to or
greater than VECTOR[START]+i.

ORDER := strict order"
  (declare (vector vector)
           ((mod #.array-dimension-limit) i start)
           ((or null (mod #.array-dimension-limit)) end))
  (labels ((recur (l r i)
             (declare ((mod #.array-dimension-limit) l r i))
             (when (= l r)
               (return-from recur (aref vector r)))
             (let* ((pivot-idx (+ l (random (+ 1 (- r l)))))
                    (mid (%hoare-partition! vector l r pivot-idx order))
                    (delta (- mid l)))
               (declare ((mod #.array-dimension-limit) pivot-idx))
               (cond ((= i delta)
                      (return-from recur (aref vector mid)))
                     ((< i delta)
                      (recur l (- mid 1) i))
                     (t
                      (recur (+ mid 1) r (- i delta 1)))))))
    (let ((end (or end (length vector))))
      (assert (< i (- end start)))
      (recur start (- end 1) i))))
