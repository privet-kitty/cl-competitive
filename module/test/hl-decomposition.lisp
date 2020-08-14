(defpackage :cp/test/hl-decomposition
  (:use :cl :fiveam :cp/hl-decomposition)
  (:import-from :cp/test/base #:base-suite))
(in-package :cp/test/hl-decomposition)
(in-suite base-suite)

(test hl-decomposition
  ;; empty graph
  (make-hl-decomposition #() :roots nil)
  (let* ((graph #((1 2) (0 3) (0 4 5) (1) (2) (2 6 8) (5 7) (6) (5) (10) (9))))
    (let ((hld (make-hl-decomposition graph :roots '(0 10))))
      (is (equalp #(9 2 6 1 1 4 2 1 1 1 2) (cp/hl-decomposition::%hld-sizes hld)))
      (is (equalp #(2 3 5 1 2 6 7 6 5 10 9)
                  (map 'vector #'car (cp/hl-decomposition::%hld-graph hld))))
      (is (equalp #(0 7 1 8 6 2 3 4 5 10 9) (cp/hl-decomposition::%hld-preords hld)))
      (is (equalp #(-1 0 0 1 2 2 5 6 5 10 -1) (cp/hl-decomposition::%hld-parents hld)))
      (is (equalp #(0 1 0 1 4 0 0 0 8 10 10) (cp/hl-decomposition::%hld-heads hld)))
      (signals two-vertices-disconnected-error
        (hld-map-path hld 0 9 (lambda (x y) (declare (ignore x y)))))
      (hld-map-path hld 0 1 (lambda (x y) (declare (ignore x y)))))))
