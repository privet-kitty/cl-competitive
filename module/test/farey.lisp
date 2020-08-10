(defpackage :cp/test/farey
  (:use :cl :fiveam :cp/farey)
  (:import-from :cp/test/base #:base-suite))
(in-package :cp/test/farey)
(in-suite base-suite)

(defun gen-farey (max-denominator from-end)
  (let (res)
    (map-farey (lambda (x y) (push (cons x y) res))
               max-denominator
               from-end)
    (nreverse res)))

(test farey
  (let ((f5 '((1 . 5) (1 . 4) (1 . 3) (2 . 5) (1 . 2) (3 . 5) (2 . 3) (3 . 4) (4 . 5))))
    (is (equal f5 (gen-farey 5 nil)))
    (is (equal (reverse f5) (gen-farey 5 t)))
    ;; trivial case
    (is (null (gen-farey 1 nil)))
    (is (null (gen-farey 1 t)))
    (is (null (gen-farey 0 nil)))
    (is (null (gen-farey 0 t)))))
