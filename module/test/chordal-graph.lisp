(eval-when (:compile-toplevel :load-toplevel :execute)
  (load "test-util")
  (load "../chordal-graph.lisp"))

(use-package :test-util)

(with-test (:name chordal-graph)
  ;; empty case
  (assert (perfect-elimination-order-p #() #()))
  (assert (perfect-elimination-order-p #(()) #(0)))
  ;; 1: 3 4 5 6
  ;; 2: 1 3 4
  ;; 3: 1 2 4
  ;; 4: 1 2 3 5 6 7
  ;; 5: 1 4 6 7
  ;; 6: 4 5 7 8
  ;; 7: 4 5 6 9 10 11
  ;; 8: 6
  ;; 9: 7 10
  ;; 10: 7 9 11 12
  ;; 11: 7 10 12
  ;; 12: 10 11
  (let ((graph #((1 2 3 4) (0 2 3) (0 1 3) (0 1 2 4 5 6) (0 3 5 6)
                 (3 4 6 7) (3 4 5 8 9 10) (5) (6 9) (6 8 10 11)
                 (6 9 11) (9 10))))
    (assert (perfect-elimination-order-p graph #(11 10 9 8 7 6 5 4 3 2 1 0)))
    (assert (not (perfect-elimination-order-p graph #(0 1 2 3 4 5 6 7 8 9 10 11))))
    ;; (assert (perfect-elimination-order-p graph #(8 11 9 10 7 1 2 5 6 3 4 0)))
    ;; (assert (make-perfect-elimination-order graph))
    )
  (let ((graph #((1 3) (0 2 3) (1 3) (0 1 2))))
    (assert (perfect-elimination-order-p graph #(0 1 2 3)))
    (assert (not (perfect-elimination-order-p graph #(1 0 2 3))))
    (assert (make-perfect-elimination-order graph))
    (assert (not (make-perfect-elimination-order #((3 1) (0 2) (1 3) (2 0)))))))
