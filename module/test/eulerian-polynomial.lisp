(defpackage :cp/test/eulerian-polynomial
  (:use :cl :fiveam :cp/eulerian-polynomial :cp/linear-sieve)
  (:import-from :cp/test/base #:base-suite))
(in-package :cp/test/eulerian-polynomial)
(in-suite base-suite)

(test make-eulerian-polynomial/hand
  (let ((minfactors (make-minfactor-table 5)))
    (is (equalp #(1) (make-eulerian-polynomial 0 minfactors)))
    (is (equalp #(1) (make-eulerian-polynomial 1 minfactors)))
    (is (equalp #(1 1) (make-eulerian-polynomial 2 minfactors)))
    (is (equalp #(1 4 1) (make-eulerian-polynomial 3 minfactors)))
    (is (equalp #(1 11 11 1) (make-eulerian-polynomial 4 minfactors)))
    (is (equalp #(1 26 66 26 1) (make-eulerian-polynomial 5 minfactors)))
    (is (equalp #(1 57 302 302 57 1) (make-eulerian-polynomial 6 minfactors)))))

(test make-eulerian-polynomial*/hand
  (let ((minfactors (make-minfactor-table 5)))
    (is (equalp #(1) (make-eulerian-polynomial* 0 minfactors)))
    (is (equalp #(0 1) (make-eulerian-polynomial* 1 minfactors)))
    (is (equalp #(0 1 1) (make-eulerian-polynomial* 2 minfactors)))
    (is (equalp #(0 1 4 1) (make-eulerian-polynomial* 3 minfactors)))
    (is (equalp #(0 1 11 11 1) (make-eulerian-polynomial* 4 minfactors)))
    (is (equalp #(0 1 26 66 26 1) (make-eulerian-polynomial* 5 minfactors)))
    (is (equalp #(0 1 57 302 302 57 1) (make-eulerian-polynomial* 6 minfactors)))))
