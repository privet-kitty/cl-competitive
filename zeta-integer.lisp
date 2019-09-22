;;;
;;; Fast Zeta/Moebius transforms w.r.t. divisor or multiple in O(nloglog(n)).
;;;

(declaim (inline divisor-transform!))
(defun divisor-transform! (vector &optional (op+ #'+) (handle-zero t))
  "Sets each VECTOR[i] to the sum of VECTOR[d] for all the divisors d of i in
O(nloglog(n)). Ignores VECTOR[0] when HANDLE-ZERO is NIL."
  (declare (vector vector))
  (let* ((n (length vector))
         (sieve (make-array n :element-type 'bit :initial-element 1)))
    (when handle-zero
      (loop for i from 1 below n
            do (setf (aref vector 0)
                     (funcall op+ (aref vector 0) (aref vector i)))))
    (loop for p from 2 below n
          when (= 1 (sbit sieve p))
          do (loop for k from 1 below (ceiling n p)
                   for pmult of-type fixnum = (* k p)
                   do (setf (sbit sieve pmult) 0)
                      (setf (aref vector pmult)
                            (funcall op+ (aref vector pmult) (aref vector k)))))
    vector))

(declaim (inline inverse-divisor-transform!))
(defun inverse-divisor-transform! (vector &optional (op- #'-) (handle-zero t))
  "Does the inverse transform of DIVISOR-TRANSFORM! in O(nloglog(n)). Ignores
VECTOR[0] when HANDLE-ZERO is NIL."
  (declare (vector vector))
  (let* ((n (length vector))
         (sieve (make-array n :element-type 'bit :initial-element 1)))
    (loop for p from 2 below n
          when (= 1 (sbit sieve p))
          do (loop for k from (- (ceiling n p) 1) downto 1
                   for pmult of-type fixnum = (* k p)
                   do (setf (sbit sieve pmult) 0)
                      (setf (aref vector pmult)
                            (funcall op- (aref vector pmult) (aref vector k)))))
    (when handle-zero
      (loop for i from 1 below n
            do (setf (aref vector 0)
                     (funcall op- (aref vector 0) (aref vector i)))))
    vector))

(declaim (inline multiple-transform!))
(defun multiple-transform! (vector &optional (op+ #'+) (handle-zero t))
  "Sets each VECTOR[i] to the sum of VECTOR[m] for all the multiples m of i in
O(nloglog(n)). (To be precise, all the multiples smaller than the length of
VECTOR.) Ignores VECTOR[0] when HANDLE-ZERO is NIL."
  (declare (vector vector))
  (let* ((n (length vector))
         (sieve (make-array n :element-type 'bit :initial-element 1)))
    (loop for p from 2 below n
          when (= 1 (sbit sieve p))
          do (loop for k from (- (ceiling n p) 1) downto 1
                   for pmult of-type fixnum = (* k p)
                   do (setf (sbit sieve pmult) 0)
                      (setf (aref vector k)
                            (funcall op+ (aref vector k) (aref vector pmult)))))
    (when handle-zero
      (loop for i from 1 below n
            do (setf (aref vector i)
                     (funcall op+ (aref vector 0) (aref vector i)))))
    vector))

(declaim (inline inverse-multiple-transform!))
(defun inverse-multiple-transform! (vector &optional (op- #'-) (handle-zero t))
  "Does the inverse transform of MULTIPLE-TRANSFORM!. Ignores VECTOR[0] when
HANDLE-ZERO is NIL."
  (declare (vector vector))
  (let* ((n (length vector))
         (sieve (make-array n :element-type 'bit :initial-element 1)))
    (when handle-zero
      (loop for i from 1 below n
            do (setf (aref vector i)
                     (funcall op- (aref vector i) (aref vector 0)))))
    (loop for p from 2 below n
          when (= 1 (sbit sieve p))
          do (loop for k from 1 below (ceiling n p)
                   for pmult of-type fixnum = (* k p)
                   do (setf (sbit sieve pmult) 0)
                      (setf (aref vector k)
                            (funcall op- (aref vector k) (aref vector pmult)))))
    vector))

;;;
;;; (Slower) Zeta/Moebius transforms w.r.t. divisor or multiple in O(nlog(n))
;;;

;; (declaim (inline divisor-transform!))
;; (defun divisor-transform! (vector &optional (op+ #'+) (handle-zero t))
;;   "Sets each VECTOR[i] to the sum of VECTOR[d] for all the divisors d of i in
;; O(nlog(n))."
;;   (declare (vector vector))
;;   (let ((n (length vector)))
;;     (when handle-zero
;;       (loop for i from 1 below n
;;             do (setf (aref vector 0)
;;                      (funcall op+ (aref vector 0) (aref vector i)))))
;;     (loop for i from (- (ceiling n 2) 1) downto 1
;;           do (loop for j from (+ i i) below n by i
;;                    do (setf (aref vector j)
;;                             (funcall op+ (aref vector i) (aref vector j)))))
;;     vector))

;; (declaim (inline inverse-divisor-transform!))
;; (defun inverse-divisor-transform! (vector &optional (op- #'-) (handle-zero t))
;;   "Does the inverse transform of DIVISOR-TRANSFORM! in O(nlog(n))."
;;   (declare (vector vector))
;;   (let ((n (length vector)))
;;     (loop for i from 1 below (ceiling n 2)
;;           do (loop for j from (+ i i) below n by i
;;                    do (setf (aref vector j)
;;                             (funcall op- (aref vector j) (aref vector i)))))
;;     (when handle-zero
;;       (loop for i from 1 below n
;;             do (setf (aref vector 0)
;;                      (funcall op- (aref vector 0) (aref vector i)))))
;;     vector))


;; (declaim (inline multiple-transform!))
;; (defun multiple-transform! (vector &optional (op+ #'+) (handle-zero t))
;;   "Sets each VECTOR[i] to the sum of VECTOR[m] for all the multiples m of i in
;; O(nlog(n)). (To be precise, all the multiples smaller than the length of
;; VECTOR.)"
;;   (declare (vector vector))
;;   (let ((n (length vector)))
;;     (loop for i from 1 below (ceiling n 2)
;;           do (loop for j from (+ i i) below n by i
;;                    do (setf (aref vector i)
;;                             (funcall op+ (aref vector i) (aref vector j)))))
;;     (when handle-zero
;;       (loop for i from 1 below n
;;             do (setf (aref vector i)
;;                      (funcall op+ (aref vector 0) (aref vector i)))))
;;     vector))


;; (declaim (inline inverse-multiple-transform!))
;; (defun inverse-multiple-transform! (vector &optional (op- #'-) (handle-zero t))
;;   "Does the inverse transform of MULTIPLE-TRANSFORM! in O(nlog(n))."
;;   (declare (vector vector))
;;   (let ((n (length vector)))
;;     (when handle-zero
;;       (loop for i from 1 below n
;;             do (setf (aref vector i)
;;                      (funcall op- (aref vector i) (aref vector 0)))))
;;     (loop for i from (- (ceiling n 2) 1) downto 1
;;           do (loop for j from (+ i i) below n by i
;;                    do (setf (aref vector i)
;;                             (funcall op- (aref vector i) (aref vector j)))))
;;     vector))
