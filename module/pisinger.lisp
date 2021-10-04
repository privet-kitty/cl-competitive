(defpackage :cp/pisinger
  (:use :cl)
  (:export #:subset-sum)
  (:documentation "Provides Pisinger's pseudo-polynomial time algorithm for
subset sum problem.

Reference:
https://atcoder.jp/contests/abc221/editorial/2743"))
(in-package :cp/pisinger)

(define-modify-macro xorf (new-value) logxor)

(declaim (ftype (function * (values simple-bit-vector &optional))
                %make-default-result))
(defun %make-default-result (n break)
  (declare (optimize (speed 3)))
  (let ((res (make-array n :element-type 'bit :initial-element 0)))
    (fill res 1 :end break)
    res))


(declaim (inline subset-sum))
(defun subset-sum (weights target &optional (restore-p t))
  (declare (vector weights)
           ((mod #.array-dimension-limit) target))
  (let ((n (length weights))
        (wmax (reduce #'max weights :initial-value 0))
        (break 0)
        (wbreak 0))
    (declare ((mod #.array-dimension-limit) wmax break wbreak))
    (loop while (and (< break n)
                     (<= (+ wbreak (aref weights break)) target))
          do (incf wbreak (aref weights break))
             (incf break))
    ;; deal with a trivial case
    (when (= wbreak target)
      (return-from subset-sum
        (if restore-p
            (%make-default-result n break)
            t)))
    (when (= break n)
      (return-from subset-sum))
    ;; TODO: ignore an item whose weight is larger than WMAX
    (let ((dp (make-array (* wmax 2) :element-type 'fixnum :initial-element -1))
          (new-dp (make-array (* wmax 2) :element-type 'fixnum :initial-element -1))
          (prevs (when restore-p
                   (make-array (list n (* wmax 2))
                               :element-type '(signed-byte 32)
                               :initial-element -1)))
          (offset (+ 1 (- target wmax))))
      (declare ((simple-array fixnum (*)) dp new-dp))
      (setf (aref dp (- wbreak offset)) break)
      (loop for i from break below n
            for wi of-type (mod #.array-dimension-limit) = (aref weights i)
            do (replace new-dp dp)
               (loop for j from (- wmax 1) downto 0
                     when (< (aref new-dp (+ j wi)) (aref new-dp j))
                     do (setf (aref new-dp (+ j wi)) (aref new-dp j))
                        (when restore-p
                          (setf (aref prevs i (+ j wi)) -2)))
               (loop for j from (- (* 2 wmax) 1) downto wmax
                     do (loop for k from (- (aref new-dp j) 1) downto (max 0 (aref dp j))
                              for wk of-type (mod #.array-dimension-limit) = (aref weights k)
                              when (< (aref new-dp (- j wk)) k)
                              do (setf (aref new-dp (- j wk)) k)
                                 (when restore-p
                                   (setf (aref prevs i (- j wk)) k))))
               (rotatef dp new-dp))
      (when (= (aref dp (- target offset)) -1)
        (return-from subset-sum))
      (unless restore-p
        (return-from subset-sum t))
      ;; restore
      (let ((res (%make-default-result n break))
            (i (- n 1))
            (j (- target offset)))
        (declare (fixnum i j))
        (loop while (>= i break)
              for prev = (aref prevs i j)
              do (case prev
                   (-2
                    (xorf (aref res i) 1)
                    (decf j (aref weights i))
                    (decf i))
                   (-1
                    (decf i))
                   (otherwise
                    (xorf (aref res prev) 1)
                    (incf j (aref weights prev)))))
        res))))
