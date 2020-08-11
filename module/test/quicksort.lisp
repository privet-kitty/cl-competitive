(defpackage :cp/test/quicksort
  (:use :cl :fiveam :cp/quicksort :cp/displace)
  (:import-from :cp/test/base #:base-suite))
(in-package :cp/test/quicksort)
(in-suite base-suite)

;; FIXME: Resorting to displacement to sort subsequence may be non-standard,
;; though it is no problem on SBCL.
(defun sort* (vector order start end)
  (sort (displace vector start end) order)
  vector)

(defun sort-by2 (vector order)
  (let* ((n (length vector))
         (pair-vector (make-array (ash n -1))))
    (dotimes (i (ash n -1))
      (setf (aref pair-vector i)
            (cons (aref vector (* 2 i))
                  (aref vector (+ 1 (* 2 i))))))
    (setq pair-vector (sort pair-vector order :key #'car))
    (dotimes (i (ash n -1))
      (let ((pair (aref pair-vector i)))
        (setf (aref vector (* 2 i)) (car pair)
              (aref vector (+ 1 (* 2 i))) (cdr pair))))
    vector))

(test quicksort
  (finishes
    (dotimes (i 100)
      (let ((vec (coerce (loop repeat i collect (random 100)) 'vector)))
        (assert (equalp (sort (copy-seq vec) #'>)
                        (quicksort! (copy-seq vec) #'>))))))
  (finishes
    (dotimes (i 1000)
      (let ((vec (coerce (loop repeat 10
                               collect (code-char (+ 97 (random 10))))
                         'vector))
            (start (random 11))
            (end (random 11)))
        (when (> start end)
          (rotatef start end))
        (assert (equalp (quicksort! (copy-seq vec) #'char< :start start :end end)
                        (sort* (copy-seq vec) #'char< start end)))))))

(test quicksort-by2
  (finishes
    (dotimes (i 1000)
      (let ((vec (coerce (loop repeat (random 100)
                               for code = (random 50)
                               for char = (code-char code)
                               append (list char code))
                         'vector)))
        (assert (equalp (quicksort-by2! (copy-seq vec) #'char<)
                        (sort-by2 (copy-seq vec) #'char<)))))))
