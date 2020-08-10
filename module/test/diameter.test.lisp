(eval-when (:compile-toplevel :load-toplevel :execute)
  (load "test-util")
  (load "../diameter.lisp"))

(use-package :test-util)

(with-test (:name relative-error)
  (let ((graph (coerce #((1 5 7) (0 3 2) (1) (4 1) (3) (0 6) (5) (0 8) (9 7) (8))
                       '(simple-array list (*)))))
    (assert (equal '(6 4 9) (multiple-value-list (find-diameter graph)))))
  (assert (equal '(0 0 0)
                 (multiple-value-list
                  (find-diameter (make-array 1 :element-type 'list :initial-element nil)))))
  (signals error (find-diameter (make-array 0 :element-type 'list))))
