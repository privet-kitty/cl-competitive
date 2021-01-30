(defpackage :cp/test/inversion-number
  (:use :cl :fiveam :cp/inversion-number)
  (:import-from :cp/test/base #:base-suite))
(in-package :cp/test/inversion-number)
(in-suite base-suite)

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

(test count-inversions!/random
  (let ((*test-dribble* nil)
        (*random-state* (sb-ext:seed-random-state 0)))
    (declare (notinline count-inversions!))
    (dotimes (len 50)
      (let ((vec (make-array len)))
        (dotimes (i (length vec))
          (setf (aref vec i)
                (cons (random 20)
                      (code-char (+ 97 (random 20))))))
        (is (= (count-inversions! (copy-seq vec) #'< :key #'car)
               (count-inversions-by-bubble-sort! (copy-seq vec) #'< :key #'car)))
        (is (= (count-inversions! (copy-seq vec) #'char< :key #'cdr)
               (count-inversions-by-bubble-sort! (copy-seq vec) #'char< :key #'cdr)))))))
