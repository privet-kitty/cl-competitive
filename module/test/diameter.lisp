(defpackage :cp/test/diameter
  (:use :cl :fiveam :cp/diameter)
  (:import-from :cp/test/base #:base-suite))
(in-package :cp/test/diameter)
(in-suite base-suite)

(test diameter
  (let ((graph (coerce #((1 5 7) (0 3 2) (1) (4 1) (3) (0 6) (5) (0 8) (9 7) (8))
                       '(simple-array list (*)))))
    (let ((path (find-diameter graph)))
      (is (or (equalp '(4 3 1 0 7 8 9) path)
              (equalp '(9 8 7 0 1 3 4) path)))))
  (is (equal '(0) (find-diameter (vector nil))))
  (is (or (equalp '(0 1) (find-diameter (vector '(1) '(0))))
          (equalp '(1 0) (find-diameter (vector '(1) '(0))))))
  (signals error (find-diameter (vector))))
