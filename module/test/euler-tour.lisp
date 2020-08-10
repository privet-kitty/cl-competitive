(defpackage :cp/test/euler-tour
  (:use :cl :fiveam :cp/euler-tour)
  (:import-from :cp/test/base #:base-suite))
(in-package :cp/test/euler-tour)
(in-suite base-suite)

(test make-euler-tour
  ;; out-tree representation
  (multiple-value-bind (tour pre post)
      (make-euler-tour #((1.1 2.1) (3.1) (4.1) () ()) :key #'round)
    (is (equalp tour #(0 1 3 1 0 2 4 2 0)))
    (is (equalp pre #(0 1 5 2 6)))
    (is (equalp post #(8 3 7 2 6))))
  ;; twin-edge representation
  (multiple-value-bind (tour pre post)
      (make-euler-tour #((1 2) (0 3) (0 4) (1) (2)) :root 0)
    (is (equalp tour #(0 1 3 1 0 2 4 2 0)))
    (is (equalp pre #(0 1 5 2 6)))
    (is (equalp post #(8 3 7 2 6))))
  (is (equalp '(#() #() #())
              (multiple-value-list (make-euler-tour #())))))
