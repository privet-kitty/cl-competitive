(eval-when (:compile-toplevel :load-toplevel :execute)
  (load "test-util")
  (load "../inversion-number.lisp"))

(use-package :test-util)

;; test
(defun count-inversions-by-bubble-sort! (vec predicate)
  "PREDICATE must be strict order."
  (loop for end from (length vec) above 0
        sum (loop with inv-count = 0
                  for i from 0 below (- end 1)
                  do (when (funcall predicate (aref vec (+ i 1)) (aref vec i))
                       (rotatef (aref vec i) (aref vec (+ i 1)))
                       (incf inv-count))
                  finally (return inv-count))))

(with-test (:name inversion-number)
  (let ((vec (make-array 200 :element-type 'fixnum)))
    (declare ((simple-array fixnum (200)) vec))
    (dotimes (i (length vec)) (setf (aref vec i) (random 20)))
    (assert (= (count-inversions! (copy-seq vec) #'<)
               (count-inversions-by-bubble-sort! (copy-seq vec) #'<)))))
