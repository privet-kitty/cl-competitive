(eval-when (:compile-toplevel :load-toplevel :execute)
  (load "test-util")
  (load "../quicksort.lisp"))

(use-package :test-util)

(with-test (:name quicksort)
  (dotimes (i 100)
    (let ((vec (coerce (loop repeat i collect (random 100)) 'vector)))
      (assert (equalp (sort (copy-seq vec) #'>)
                      (quicksort! (copy-seq vec) #'>))))))
