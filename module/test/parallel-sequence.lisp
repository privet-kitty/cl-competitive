(defpackage :cp/test/parallel-sequence
  (:use :cl :fiveam :cp/parallel-sort :cp/parallel-shuffle)
  (:import-from :cp/test/base #:base-suite))
(in-package :cp/test/parallel-sequence)
(in-suite base-suite)

;; test both of source-transform and full call
(defmacro with-not/inline (&body body)
  `(progn (locally (declare (notinline parallel-shuffle! parallel-sort!))
            ,@body)
          ,@body))

(test parallel-shuffle!
  (let ((vector1 (coerce (loop for v below 26 collect (code-char (+ v 65))) 'vector))
        (vector2 (coerce (loop for v below 26 collect (code-char (+ v 97))) 'vector))
        (vector3 (coerce (loop for v below 26 collect v) 'vector)))
    (finishes
      (dotimes (i 200)
        (with-not/inline
            (parallel-shuffle! vector1 vector2 vector3)
          (dotimes (i 26)
            (assert (= (- (char-code (aref vector1 i)) 65)
                       (- (char-code (aref vector2 i)) 97)
                       (aref vector3 i)))))))))

(test parallel-sort!
  (let ((vector1 (coerce (loop for v below 26 collect (code-char (+ v 65))) 'vector))
        (vector2 (coerce (loop for v below 26 collect (code-char (+ v 97))) 'vector))
        (vector3 (coerce (loop for v below 26 collect v) 'vector)))
    (finishes
      (dotimes (i 200)
        (with-not/inline
            (parallel-shuffle! vector1 vector2 vector3)
          (parallel-sort! vector1 #'char< vector2 vector3)
          (dotimes (i 26)
            (assert (= (- (char-code (aref vector1 i)) 65)
                       (- (char-code (aref vector2 i)) 97)
                       (aref vector3 i))))
          (dotimes (i 25)
            (assert (char< (aref vector1 i) (aref vector1 (+ i 1))))
            (assert (char< (aref vector2 i) (aref vector2 (+ i 1))))
            (assert (< (aref vector3 i) (aref vector3 (+ i 1))))))))))
