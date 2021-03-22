(defpackage :cp/test/binom-mod-small
  (:use :cl :fiveam :cp/binom-mod-small #:cp/binom-quadratic #:cp/linear-sieve)
  (:import-from :cp/test/base #:base-suite))
(in-package :cp/test/binom-mod-small)
(in-suite base-suite)

(test binom-mod-small
  (let ((mtable (make-minfactor-table 1001))
        (*test-dribble* nil))
    (loop for mod in (list 1 2 3 4 5 6 7 8 9 10
                           100
                           (* 3 3 3 3 3)
                           (* 2 2 2 3 3 3))
          for table = (make-binom-table 200 '(unsigned-byte 8)
                                        (lambda (x y)
                                          (declare ((unsigned-byte 8) x y))
                                          (mod (+ x y) mod)))
          for pnodes = (make-pnodes (factorize mod mtable))
          do (dotimes (i 80)
               (dotimes (j 80)
                 (is (= (binom i j pnodes)
                        (aref table i j))))))))
