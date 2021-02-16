(defpackage :cp/test/round-robin
  (:use :cl :fiveam :cp/round-robin)
  (:import-from :cp/test/base #:base-suite))
(in-package :cp/test/round-robin)
(in-suite base-suite)

(test round-robin
  (let ((*test-dribble* nil))
    (finishes (round-robin-map 0 (lambda (&rest _)
                                   (declare (ignore _))
                                   (error "Huh?"))))
    (loop for n to 20 by 2
          for round = 0
          for marked = (make-array (list n n) :element-type 'bit :initial-element 0)
          do (round-robin-map
              n
              (lambda (vector round*)
                (is (= round round*))
                (is (= n (length (remove-duplicates vector))))
                (dotimes (i n)
                  (setf (aref marked i (aref vector i)) 1))
                (incf round)))
             (dotimes (i n)
               (dotimes (j n)
                 (is (= (if (= i j) 0 1)
                        (aref marked i j))))))))
