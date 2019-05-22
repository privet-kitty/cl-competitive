(eval-when (:compile-toplevel :load-toplevel :execute)
  (load "test-util")
  (load "../adjacent-duplicates.lisp"))

(use-package :test-util)

(with-test (:name delete-adjacent-duplicates)
  (assert (equalp #() (delete-adjacent-duplicates #())))
  (assert (equalp #(1 2 3 1 2) (delete-adjacent-duplicates #(1 2 2 3 3 1 1 1 2 2))))
  (assert (equalp #(1 2 3) (delete-adjacent-duplicates #(1 1 2 3))))
  (assert (equalp #(0 0.0 0) (delete-adjacent-duplicates #(0 0.0 0))))
  (assert (equalp #(0) (delete-adjacent-duplicates #(0 0.0 0) :test #'=))))

(with-test (:name map-adjacent-duplicates)
  (map-adjacent-duplicates (lambda (x) (error "Must not be called.")) #())
  (let ((result '((1 . 1) (2 . 2) (3 . 2) (1 . 3) (2 . 2))))
    (map-adjacent-duplicates
     (lambda (x y) (assert (equal (cons x y) (pop result))))
     #(1 2 2 3 3 1 1 1 2 2)))
  (map-adjacent-duplicates
   (lambda (x y) (assert (and (= 0 x) (= 3 y))))
   #(0 0.0 0)
   :test #'=))
