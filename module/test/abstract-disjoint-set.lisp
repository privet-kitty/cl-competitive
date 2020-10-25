(defpackage :cp/test/abstract-disjoint-set
  (:use :cl :fiveam :cp/abstract-disjoint-set)
  (:import-from :cp/test/base #:base-suite))
(in-package :cp/test/abstract-disjoint-set)
(in-suite base-suite)

(define-disjoint-set dset
  :op (lambda (s1 s2) (concatenate 'simple-base-string s1 s2))
  :identity ""
  :element-type string
  :conc-name d-)

(test abstract-disjoint-set
  (let ((dset (make-dset 5 (coerce #("a" "b" "c" "d" "e") '(simple-array string (*))))))
    (is (/= (d-root dset 0) (d-root dset 1)))
    (loop for i below 5
          for size in '(1 1 1 1 1)
          for s in '("a" "b" "c" "d" "e")
          do (is (equal (d-ref dset i) s))
             (is (= (d-size dset i) size)))
    (d-unite! dset 0 1)
    (is (= (d-root dset 0) (d-root dset 1)))
    (is (not (d-connected-p dset 3 4)))
    (loop for i below 5
          for size in '(2 2 1 1 1)
          for s in '("ab" "ab" "c" "d" "e")
          do (is (equal (d-ref dset i) s))
             (is (= (d-size dset i) size)))
    (d-unite! dset 3 4)
    (is (d-connected-p dset 3 4))
    (is (not (d-connected-p dset 1 3)))
    (loop for i below 5
          for size in '(2 2 1 2 2)
          for s in '("ab" "ab" "c" "de" "de")
          do (is (equal (d-ref dset i) s))
             (is (= (d-size dset i) size)))
    (d-unite! dset 0 4)
    (is (d-connected-p dset 1 3))
    (loop for i below 5
          for size in '(4 4 1 4 4)
          for s in '("abde" "abde" "c" "abde" "abde")
          do (is (equal (d-ref dset i) s))
             (is (= (d-size dset i) size))))
  (finishes (make-dset 0))
  (finishes (make-dset 1)))
