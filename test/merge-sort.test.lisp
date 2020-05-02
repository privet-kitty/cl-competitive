(eval-when (:compile-toplevel :load-toplevel :execute)
  (load "test-util")
  (load "../merge-sort.lisp")
  (load "../displace.lisp"))

(use-package :test-util)

;; FIXME: Resorting to displacement to sort subsequence may be non-standard,
;; though it is no problem on SBCL.
(defun sort* (vector order start end key)
  (sort (displace vector start end) order :key key)
  vector)

(with-test (:name merge-sort)
  (dotimes (i 100)
    (let ((vec (coerce (loop repeat i collect (random 100)) 'vector)))
      (assert (equalp (sort (copy-seq vec) #'>)
                      (merge-sort! (copy-seq vec) #'>)))))
  (dotimes (i 1000)
    (let ((vec (coerce (loop repeat 10
                             collect (code-char (+ 97 (random 10))))
                       'vector))
          (start (random 11))
          (end (random 11)))
      (when (> start end)
        (rotatef start end))
      (assert (equalp (merge-sort! (copy-seq vec) #'<
                                   :start start :end end :key #'char-code)
                      (sort* (copy-seq vec) #'< start end #'char-code))))))
