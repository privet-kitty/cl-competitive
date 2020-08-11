(defpackage :cp/test/with-cache
  (:use :cl :fiveam :cp/with-cache)
  (:import-from :cp/test/base #:base-suite))
(in-package :cp/test/with-cache)
(in-suite base-suite)

(test with-cache
  ;; array
  (let (call-stack)
    (with-cache (:array (3 3) :initial-element -1)
      (labels ((add (x y)
                 (push (cons x y) call-stack)
                 (if (zerop x)
                     y
                     (+ 1 (add (- x 1) y)))))
        (is (= 3 (add 1 2)))
        (is (equal '((1 . 2) (0 . 2)) (reverse call-stack)))
        (setq call-stack nil)
        (is (= 4 (add 2 2)))
        (is (equal '((2 . 2)) (reverse call-stack)))
        (setq call-stack nil)
        (is (= 3 (add 1 2)))
        (is (null (reverse call-stack))))))
  ;; hash-table
  (let (call-stack)
    (with-cache (:hash-table :test #'equal)
      (labels ((add (x y)
                 (push (cons x y) call-stack)
                 (if (zerop x)
                     y
                     (+ 1 (add (- x 1) y)))))
        (is (= 3 (add 1 2)))
        (is (equal '((1 . 2) (0 . 2)) (reverse call-stack)))
        (setq call-stack nil)
        (is (= 4 (add 2 2)))
        (is (equal '((2 . 2)) (reverse call-stack)))
        (setq call-stack nil)
        (is (= 3 (add 1 2)))
        (is (null (reverse call-stack)))))))
