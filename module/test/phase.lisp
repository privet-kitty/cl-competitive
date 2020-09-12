(defpackage :cp/test/phase
  (:use :cl :fiveam :cp/phase)
  (:import-from :cp/test/base #:base-suite))
(in-package :cp/test/phase)
(in-suite base-suite)

(test phase
  (let ((vector (vector #c(-10 -1) #c(-1 -1) #c(-1 -2) #c(0 -1) #c(1 -1) #c(2 -1)
                        #c(1 0) #c(1 1) #c(1 2) #c(0 1) #c(-1 1) #c(-2 1) #c(-1 0))))
    (dotimes (i (length vector))
      (dotimes (j (length vector))
        (if (< i j)
            (is (phase< (aref vector i) (aref vector j)))
            (is (not (phase< (aref vector i) (aref vector j)))))))))
