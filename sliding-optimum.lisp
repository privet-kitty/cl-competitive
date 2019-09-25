;;;
;;; Sliding window optimum
;;;

(declaim (inline calc-sliding-opt))
(defun calc-sliding-opt (vector width order)
  "Computes all the minima (or maxima) of the subsequences of the given width,
VECTOR[i, i+WIDTH), in O(n). Returns a vector, whose i-th element is the
minimum (or maximum) of the subsequence beginning with the index i.

ORDER := strict order (#'< corresponds to slide min. and #'> to slide max.)"
  (declare (vector vector)
           ((integer 1 #.most-positive-fixnum) width))
  (let* ((n (length vector))
         (deq (make-array n :element-type '(integer 0 #.most-positive-fixnum)))
         (front-pos 0)
         (end-pos -1)
         (l 0)
         (r 0)
         (res (make-array (+ 1 (- n width)) :element-type (array-element-type vector))))
    (declare ((integer -1 #.most-positive-fixnum) front-pos end-pos l r))
    (assert (<= width n))
    (labels ((push-back (x)
               (incf end-pos)
               (setf (aref deq end-pos) x))
             (pop-back ()
               (decf end-pos))
             (pop-front ()
               (incf front-pos))
             (peek-back ()
               (aref deq end-pos))
             (peek-front ()
               (aref deq front-pos))
             (extend ()
               (loop while (and (<= front-pos end-pos)
                                (not (funcall order
                                              (aref vector (peek-back))
                                              (aref vector r))))
                     do (pop-back))
               (push-back r)
               (incf r))
             (shrink ()
               (when (= (aref deq front-pos) l)
                 (pop-front))
               (incf l)))
      (declare (inline push-back pop-back pop-front peek-back peek-front))
      (loop for i below width
            do (extend))
      (loop for i from width below n
            do (setf (aref res (- i width))
                     (aref vector (peek-front)))
               (extend)
               (shrink)
            finally (setf (aref res (- i width))
                          (aref vector (peek-front))))
      res)))
