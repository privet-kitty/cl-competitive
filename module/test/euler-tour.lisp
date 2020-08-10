(eval-when (:compile-toplevel :load-toplevel :execute)
  (load "test-util")
  (load "../euler-tour.lisp"))

(use-package :test-util)

(with-test (:name make-euler-tour)
  ;; out-tree representation
  (multiple-value-bind (tour pre post)
      (make-euler-tour #((1.1 2.1) (3.1) (4.1) () ()) :key #'round)
    (assert (equalp tour #(0 1 3 1 0 2 4 2 0)))
    (assert (equalp pre #(0 1 5 2 6)))
    (assert (equalp post #(8 3 7 2 6))))
  ;; twin-edge representation
  (multiple-value-bind (tour pre post)
      (make-euler-tour #((1 2) (0 3) (0 4) (1) (2)) :root 0)
    (assert (equalp tour #(0 1 3 1 0 2 4 2 0)))
    (assert (equalp pre #(0 1 5 2 6)))
    (assert (equalp post #(8 3 7 2 6))))
  (assert (equalp '(#() #() #())
                  (multiple-value-list (make-euler-tour #())))))
