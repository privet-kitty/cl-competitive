(eval-when (:compile-toplevel :load-toplevel :execute)
  (load "test-util")
  (load "../warshall-floyd.lisp"))

(use-package :test-util)

(defconstant +inf+ most-positive-fixnum)

(with-test (:name warshall-floyd)
  ;; from https://www.geeksforgeeks.org/floyd-warshall-algorithm-dp-16/
  (let ((mat (make-array '(4 4) :initial-contents `((0 5 ,+inf+ 10)
                                                    (,+inf+ 0 3 ,+inf+)
                                                    (,+inf+ ,+inf+ 0 1)
                                                    (,+inf+ ,+inf+ ,+inf+ 0)))))
    (assert (equalp (warshall-floyd! mat)
                    (make-array '(4 4)
                                :initial-contents `((0 5 8 9)
                                                    (,+inf+ 0 3 4)
                                                    (,+inf+ ,+inf+ 0 1)
                                                    (,+inf+ ,+inf+ ,+inf+ 0))))))
  ;; empty-case
  (assert (equalp #2a() (warshall-floyd! (make-array (list 0 0))))))
