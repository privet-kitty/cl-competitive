(eval-when (:compile-toplevel :load-toplevel :execute)
  (load "test-util")
  (load "../hl-decomposition.lisp"))

(use-package :test-util)

(with-test (:name hl-decomposition)
  ;; empty graph
  (make-hl-decomposition #() :roots nil)
  (let* ((graph #((1 2) (0 3) (0 4 5) (1) (2) (2 6 8) (5 7) (6) (5) (10) (9))))
    (let ((hld (make-hl-decomposition graph :roots '(0 10))))
      (assert (equalp #(9 2 6 1 1 4 2 1 1 1 2) (%hld-sizes hld)))
      (assert (equalp #(2 3 5 1 2 6 7 6 5 10 9)
                      (map 'vector #'car (%hld-graph hld))))
      (assert (equalp #(0 7 1 8 6 2 3 4 5 10 9) (%hld-preords hld)))
      (assert (equalp #(-1 0 0 1 2 2 5 6 5 10 -1) (%hld-parents hld)))
      (assert (equalp #(0 1 0 1 4 0 0 0 8 10 10) (%hld-heads hld)))
      (signals two-vertices-disconnected-error
        (hld-map-path hld 0 9 (lambda (x y) (declare (ignore x y)))))
      (hld-map-path hld 0 1 (lambda (x y) (declare (ignore x y)))))))
