(eval-when (:compile-toplevel :load-toplevel :execute)
  (load "test-util")
  (load "../farey.lisp"))

(use-package :test-util)

(defun gen-farey (max-denominator from-end)
  (let (res)
    (map-farey (lambda (x y) (push (cons x y) res))
               max-denominator
               from-end)
    (nreverse res)))

(with-test (:name farey)
  (let ((f5 '((1 . 5) (1 . 4) (1 . 3) (2 . 5) (1 . 2) (3 . 5) (2 . 3) (3 . 4) (4 . 5))))
    (assert (equal f5 (gen-farey 5 nil)))
    (assert (equal (reverse f5) (gen-farey 5 t)))
    ;; trivial case
    (assert (null (gen-farey 1 nil)))
    (assert (null (gen-farey 1 t)))
    (assert (null (gen-farey 0 nil)))
    (assert (null (gen-farey 0 t)))))
