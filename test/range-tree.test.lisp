(eval-when (:compile-toplevel :load-toplevel :execute)
  (load "test-util")
  (load "../range-tree.lisp"))

(use-package :test-util)

;;7...#..
;;6...#..
;;5....#.
;;4.#.#..
;;3......
;;2.....#
;;1.#..#.
;;0......
;; 012345
(with-test (:name range-tree)
  (let* ((points #((1 . 1) (1 . 4) (3 . 4) (3 . 6) (3 . 7) (4 . 1) (4 . 4) (5 . 2)))
         (rt (make-range-tree 8
                              (lambda (i) (car (aref points i)))
                              (lambda (i) (cdr (aref points i)))
                              (constantly 1))))
    (assert (= 8 (rt-count rt nil nil nil nil)))
    (assert (= 8 (rt-count rt 1 1 nil nil)))
    (assert (= 8 (rt-count rt 1 1 6 8)))
    (assert (= 6 (rt-count rt 1 1 5 7)))
    (assert (= 1 (rt-count rt 1 4 3 7)))
    (assert (= 4 (rt-count rt 1 4 4 8)))
    (assert (= 3 (rt-count rt 1 4 4 7)))
    (assert (= 2 (rt-count rt 1 4 4 6)))
    (assert (= 2 (rt-count rt 1 4 4 5)))
    (assert (= 0 (rt-count rt 1 1 nil 1)))
    (assert (= 2 (rt-count rt 4 1 5 5)))
    (assert (= 1 (rt-count rt 3 4 4 5)))

    (assert (= 8 (rt-query rt nil nil nil nil)))
    (assert (= 8 (rt-query rt 1 1 nil nil)))
    (assert (= 8 (rt-query rt 1 1 6 8)))
    (assert (= 6 (rt-query rt 1 1 5 7)))
    (assert (= 1 (rt-query rt 1 4 3 7)))
    (assert (= 4 (rt-query rt 1 4 4 8)))
    (assert (= 3 (rt-query rt 1 4 4 7)))
    (assert (= 2 (rt-query rt 1 4 4 6)))
    (assert (= 2 (rt-query rt 1 4 4 5)))
    (assert (= 0 (rt-query rt 1 1 nil 1)))
    (assert (= 2 (rt-query rt 4 1 5 5)))
    (assert (= 1 (rt-query rt 3 4 4 5)))))
