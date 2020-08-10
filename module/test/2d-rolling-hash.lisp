(defpackage :cp/test/2d-rolling-hash
  (:use :cl :fiveam :cp/2d-rolling-hash)
  (:import-from :cp/test/base #:base-suite))
(in-package :cp/test/2d-rolling-hash)
(in-suite base-suite)

(5am:test 2d-rolling-hash
  (locally (declare (notinline rhash2d-matrix-hash))
    (let ((rhash (make-rhash2d #2a((1 0 2 3)
                                   (1 3 1 4)
                                   (5 1 0 2)
                                   (0 1 3 1))
                               2 3)))
      (is (= (aref rhash 0 0)
             (aref rhash 2 1)))
      (is (/= (aref rhash 0 0)
              (aref rhash 0 1)))
      (is (= (rhash2d-matrix-hash #2a((0 2 3) (3 1 4)))
             (aref rhash 0 1)))
      ;; zero case
      (finishes (make-rhash2d #2a() 0 0)))))
