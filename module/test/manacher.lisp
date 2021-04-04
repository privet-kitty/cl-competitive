(defpackage :cp/test/manacher
  (:use :cl :fiveam :cp/manacher)
  (:import-from :cp/test/base #:base-suite))
(in-package :cp/test/manacher)
(in-suite base-suite)

(test manacher
  (is (equalp #(1 2 1 4 1 2 3 2 1)
              (%manacher "abaaababa")))
  (is (equalp #(1) (%manacher "a")))
  (is (equalp #(1 1 1)
              (%manacher (vector (list 1 2 3) (list 1 2 3) (list 1 2 3)))))
  (is (equalp #(1 2 1)
              (%manacher (vector (list 1 2 3) (list 1 2 3) (list 1 2 3))
                         #'equal)))
  (is (equalp #() (%manacher ""))))
