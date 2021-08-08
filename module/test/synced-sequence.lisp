(defpackage :cp/test/synced-sequence
  (:use :cl :fiveam :cp/synced-sort :cp/synced-shuffle)
  (:import-from :cp/test/base #:base-suite))
(in-package :cp/test/synced-sequence)
(in-suite base-suite)

;; test both of source-transform and full call
(defmacro with-not/inline (&body body)
  `(progn (locally (declare (notinline synced-shuffle! synced-sort!))
            ,@body)
          ,@body))

(test synced-shuffle!
  (let ((vector1 (coerce (loop for v below 26 collect (code-char (+ v 65))) 'vector))
        (vector2 (coerce (loop for v below 26 collect (code-char (+ v 97))) 'vector))
        (vector3 (coerce (loop for v below 26 collect v) 'vector)))
    (finishes
      (dotimes (i 200)
        (with-not/inline
            (synced-shuffle! vector1 vector2 vector3)
          (dotimes (i 26)
            (assert (= (- (char-code (aref vector1 i)) 65)
                       (- (char-code (aref vector2 i)) 97)
                       (aref vector3 i)))))))))

(test synced-sort!
  (let ((vector1 (coerce (loop for v below 26 collect (code-char (+ v 65))) 'vector))
        (vector2 (coerce (loop for v below 26 collect (code-char (+ v 97))) 'vector))
        (vector3 (coerce (loop for v below 26 collect v) 'vector)))
    (finishes
      (dotimes (i 200)
        (with-not/inline
            (synced-shuffle! vector1 vector2 vector3)
          (synced-sort! vector1 #'char< vector2 vector3)
          (dotimes (i 26)
            (assert (= (- (char-code (aref vector1 i)) 65)
                       (- (char-code (aref vector2 i)) 97)
                       (aref vector3 i))))
          (dotimes (i 25)
            (assert (char< (aref vector1 i) (aref vector1 (+ i 1))))
            (assert (char< (aref vector2 i) (aref vector2 (+ i 1))))
            (assert (< (aref vector3 i) (aref vector3 (+ i 1))))))))))
