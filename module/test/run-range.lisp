(defpackage :cp/test/run-range
  (:use :cl :fiveam :cp/run-range)
  (:import-from :cp/test/base #:base-suite))
(in-package :cp/test/run-range)
(in-suite base-suite)

(test map-run-range
  (declare (notinline map-run-range))
  ;; vector
  (finishes (map-run-range (lambda (&rest args) (error "Must not be called.")) #()))
  (let ((result '((3 0 1))))
    (map-run-range (lambda (&rest args) (is (equal args (pop result)))) #(3)))
  (let ((result '((1 0 1) (2 1 3) (3 3 5) (1 5 8) (2 8 10))))
    (map-run-range (lambda (&rest args) (is (equal args (pop result))))
                   #(1 2 2 3 3 1 1 1 2 2)))
  (map-run-range (lambda (&rest args) (is (equal args '(0 0 3))))
                 #(0 0.0 0)
                 :test #'=)
  ;; list
  (finishes (map-run-range (lambda (&rest args) (error "Must not be called.")) nil))
  (let ((result '((3 0 1))))
    (map-run-range (lambda (&rest args) (is (equal args (pop result)))) '(3)))
  (let ((result '((1 0 1) (2 1 3) (3 3 5) (1 5 8) (2 8 10))))
    (map-run-range (lambda (&rest args) (is (equal args (pop result))))
                   '(1 2 2 3 3 1 1 1 2 2)))
  (map-run-range (lambda (&rest args) (is (equal args '(0 0 3))))
                 '(0 0.0 0)
                 :test #'=))
