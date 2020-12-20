(defpackage :cp/test/merge-sort
  (:use :cl :fiveam :cp/merge-sort :cp/displace)
  (:import-from :cp/test/base #:base-suite))
(in-package :cp/test/merge-sort)
(in-suite base-suite)

;; FIXME: Resorting to displacement to sort subsequence may be non-standard,
;; though it is no problem on SBCL.
(defun sort* (vector order start end key)
  (sort (displace vector start end) order :key key)
  vector)

(test merge-sort/hand
  (declare (notinline merge-sort!))
  (is (equalp #() (merge-sort! (vector) #'<)))
  (is (equalp #(3 1 2) (merge-sort! (vector 3 1 2) #'> :start 3 :end 3)))
  (is (equalp #(3 1 2) (merge-sort! (vector 3 1 2) #'> :start 2 :end 3)))
  (is (equalp #(3 2 1) (merge-sort! (vector 3 1 2) #'> :start 1 :end 3)))
  (is (equalp #(#\a) (merge-sort! (vector #\a) #'< :key #'char-code)))
  (is (equalp #(#\a) (merge-sort! (vector #\a) #'> :key #'char-code)))
  (is (equalp #(#\a #\b) (merge-sort! (vector #\b #\a) #'< :key #'char-code)))
  (is (equalp #(#\b #\a) (merge-sort! (vector #\b #\a) #'> :key #'char-code))))

(test merge-sort/random
  (declare (notinline merge-sort!))
  (finishes
    (dotimes (i 100)
      (let ((vec (coerce (loop repeat i collect (random 100)) 'vector)))
        (assert (equalp (sort (copy-seq vec) #'>)
                        (merge-sort! (copy-seq vec) #'>))))))
  (finishes
    (dotimes (i 1000)
      (let ((vec (coerce (loop repeat 20
                               collect (code-char (+ 97 (random 10))))
                         'vector))
            (start (random 11))
            (end (random 11)))
        (when (> start end)
          (rotatef start end))
        (assert (equalp (merge-sort! (copy-seq vec) #'<
                                     :start start :end end :key #'char-code)
                        (sort* (copy-seq vec) #'< start end #'char-code)))))))
