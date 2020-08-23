(defpackage :cp/test/undoable-disjoint-set
  (:use :cl :fiveam :cp/undoable-disjoint-set)
  (:import-from :cp/test/base #:base-suite))
(in-package :cp/test/undoable-disjoint-set)
(in-suite base-suite)

(test undoable-disjoint-set
  (let ((dset (make-undoable-disjoint-set 5)))
    ;; state 0
    (is (/= (uds-root dset 0) (uds-root dset 1)))
    (is (= 1 (uds-size dset 0) (uds-root dset 1)))
    (uds-unite! dset 0 1)
    ;; state 1
    (is (= (uds-root dset 0) (uds-root dset 1)))
    (is (not (uds-connected-p dset 3 4)))
    (is (equal '(2 2 1 1 1)
               (loop for x below 5 collect (uds-size dset x))))
    (uds-snapshot! dset)
    (uds-unite! dset 3 4)
    ;; state 2
    (is (uds-connected-p dset 3 4))
    (is (not (uds-connected-p dset 1 3)))
    (is (equal '(2 2 1 2 2)
               (loop for x below 5 collect (uds-size dset x))))
    (uds-unite! dset 0 4)
    ;; state 3
    (is (uds-connected-p dset 1 3))
    (is (equal '(4 4 1 4 4)
               (loop for x below 5 collect (uds-size dset x))))
    (uds-unite! dset 0 4)
    ;; state 4 (= state 3)
    (is (uds-connected-p dset 1 3))
    (is (equal '(4 4 1 4 4)
               (loop for x below 5 collect (uds-size dset x))))
    (uds-undo! dset)
    ;; state 3
    (is (uds-connected-p dset 1 3))
    (is (equal '(4 4 1 4 4)
               (loop for x below 5 collect (uds-size dset x))))
    (uds-undo! dset)
    ;; state 2
    (is (uds-connected-p dset 3 4))
    (is (not (uds-connected-p dset 1 3)))
    (is (equal '(2 2 1 2 2)
               (loop for x below 5 collect (uds-size dset x))))
    (uds-rollback! dset)
    ;; state 1
    (is (= (uds-root dset 0) (uds-root dset 1)))
    (is (not (uds-connected-p dset 3 4)))
    (is (equal '(2 2 1 1 1)
               (loop for x below 5 collect (uds-size dset x))))
    ;; undo empty record
    (signals error (uds-undo! dset))
    (is (null (uds-undo! dset nil))))
  (finishes (make-undoable-disjoint-set 0))
  (finishes (make-undoable-disjoint-set 1)))
