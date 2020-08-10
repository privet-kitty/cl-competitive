(defpackage :cp/test/inversion-number
  (:use :cl :fiveam :cp/inversion-number)
  (:import-from :cp/test/base #:base-suite))
(in-package :cp/test/inversion-number)
(in-suite base-suite)

;; test
(defun count-inversions-by-bubble-sort! (vec predicate &key (key #'identity))
  "PREDICATE must be strict order."
  (loop for end from (length vec) above 0
        sum (loop with inv-count = 0
                  for i from 0 below (- end 1)
                  do (when (funcall predicate
                                    (funcall key (aref vec (+ i 1)))
                                    (funcall key (aref vec i)))
                       (rotatef (aref vec i) (aref vec (+ i 1)))
                       (incf inv-count))
                  finally (return inv-count))))

(declaim (notinline count-inversions!))

(test inversion-number
  (declare (notinline count-inversions!))
  (let ((vec (make-array 200))
        (state (sb-ext:seed-random-state 0)))
    (dotimes (i (length vec))
      (setf (aref vec i) (cons (random 20 state) (random 20 state))))
    (is (= (count-inversions! (copy-seq vec) #'< :key #'cdr)
           (count-inversions-by-bubble-sort! (copy-seq vec) #'< :key #'cdr)))
    (is (= (count-inversions! (copy-seq vec) #'< :key #'car)
           (count-inversions-by-bubble-sort! (copy-seq vec) #'< :key #'car)))))
