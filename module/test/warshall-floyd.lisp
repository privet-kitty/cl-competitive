(defpackage :cp/test/warshall-floyd
  (:use :cl :fiveam :cp/warshall-floyd)
  (:import-from :cp/test/base #:base-suite))
(in-package :cp/test/warshall-floyd)
(in-suite base-suite)

(defconstant +inf+ most-positive-fixnum)

(test warshall-floyd
  ;; from https://www.geeksforgeeks.org/floyd-warshall-algorithm-dp-16/
  (let ((mat (make-array '(4 4) :initial-contents `((0 5 ,+inf+ 10)
                                                    (,+inf+ 0 3 ,+inf+)
                                                    (,+inf+ ,+inf+ 0 1)
                                                    (,+inf+ ,+inf+ ,+inf+ 0)))))
    (is (equalp (warshall-floyd! mat)
                (make-array '(4 4)
                            :initial-contents `((0 5 8 9)
                                                (,+inf+ 0 3 4)
                                                (,+inf+ ,+inf+ 0 1)
                                                (,+inf+ ,+inf+ ,+inf+ 0))))))
  ;; empty-case
  (is (equalp #2a() (warshall-floyd! (make-array (list 0 0))))))
