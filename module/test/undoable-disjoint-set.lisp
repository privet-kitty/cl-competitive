(defpackage :cp/test/undoable-disjoint-set
  (:use :cl :fiveam :cp/undoable-disjoint-set)
  (:import-from :cp/test/base #:base-suite))
(in-package :cp/test/undoable-disjoint-set)
(in-suite base-suite)

(define-undoable-disjoint-set undoable-disjoint-set
  :element-type double-float
  :identity 0d0
  :operator #'max
  :conc-name uds-)

;; FIXME: more organized manual test or randomized test
(test undoable-disjoint-set/hand
  (let* ((contents (make-array 5
                               :element-type 'double-float
                               :initial-contents #(5d0 1d0 7d0 3d0 9d0)))
         (dset (make-undoable-disjoint-set 5 :initial-contents contents)))
    ;; state 0
    (is (/= (uds-root dset 0) (uds-root dset 1)))
    (is (= 1 (uds-size dset 0) (uds-root dset 1)))
    (is (equal '(1 1 1 1 1)
               (loop for x below 5 collect (uds-size dset x))))
    (is (equal '(5d0 1d0 7d0 3d0 9d0)
               (loop for x below 5 collect (uds-ref dset x))))
    (uds-unite! dset 0 2)
    ;; state 1a
    (is (= (uds-root dset 0) (uds-root dset 2)))
    (is (not (uds-connected-p dset 3 4)))
    (is (equal '(2 1 2 1 1)
               (loop for x below 5 collect (uds-size dset x))))
    (is (equal '(7d0 1d0 7d0 3d0 9d0)
               (loop for x below 5 collect (uds-ref dset x))))
    (uds-undo! dset)
    ;; state 0
    (is (/= (uds-root dset 0) (uds-root dset 1)))
    (is (= 1 (uds-size dset 0) (uds-root dset 1)))
    (is (equal '(1 1 1 1 1)
               (loop for x below 5 collect (uds-size dset x))))
    (is (equal '(5d0 1d0 7d0 3d0 9d0)
               (loop for x below 5 collect (uds-ref dset x))))
    (uds-unite! dset 0 1)
    ;; state 1b
    (is (= (uds-root dset 0) (uds-root dset 1)))
    (is (not (uds-connected-p dset 3 4)))
    (is (equal '(2 2 1 1 1)
               (loop for x below 5 collect (uds-size dset x))))
    (is (equal '(5d0 5d0 7d0 3d0 9d0)
               (loop for x below 5 collect (uds-ref dset x))))
    (uds-snapshot! dset)
    (uds-unite! dset 3 4)
    ;; state 2
    (is (uds-connected-p dset 3 4))
    (is (not (uds-connected-p dset 1 3)))
    (is (equal '(2 2 1 2 2)
               (loop for x below 5 collect (uds-size dset x))))
    (is (equal '(5d0 5d0 7d0 9d0 9d0)
               (loop for x below 5 collect (uds-ref dset x))))
    (uds-unite! dset 0 4)
    ;; state 3
    (is (uds-connected-p dset 1 3))
    (is (equal '(4 4 1 4 4)
               (loop for x below 5 collect (uds-size dset x))))
    (is (equal '(9d0 9d0 7d0 9d0 9d0)
               (loop for x below 5 collect (uds-ref dset x))))
    (uds-unite! dset 0 4)
    ;; state 4 (= state 3)
    (is (uds-connected-p dset 1 3))
    (is (equal '(4 4 1 4 4)
               (loop for x below 5 collect (uds-size dset x))))
    (is (equal '(9d0 9d0 7d0 9d0 9d0)
               (loop for x below 5 collect (uds-ref dset x))))
    (setf (uds-ref dset 0) 100d0)
    ;; state 5
    (is (equal '(4 4 1 4 4)
               (loop for x below 5 collect (uds-size dset x))))
    (is (equal '(100d0 100d0 7d0 100d0 100d0)
               (loop for x below 5 collect (uds-ref dset x))))
    (uds-undo! dset)
    ;; state 4
    (is (uds-connected-p dset 1 3))
    (is (equal '(4 4 1 4 4)
               (loop for x below 5 collect (uds-size dset x))))
    (is (equal '(9d0 9d0 7d0 9d0 9d0)
               (loop for x below 5 collect (uds-ref dset x))))
    (uds-undo! dset)
    ;; state 3
    (is (uds-connected-p dset 1 3))
    (is (equal '(4 4 1 4 4)
               (loop for x below 5 collect (uds-size dset x))))
    (is (equal '(9d0 9d0 7d0 9d0 9d0)
               (loop for x below 5 collect (uds-ref dset x))))
    (uds-undo! dset)
    ;; state 2
    (is (uds-connected-p dset 3 4))
    (is (not (uds-connected-p dset 1 3)))
    (is (equal '(2 2 1 2 2)
               (loop for x below 5 collect (uds-size dset x))))
    (is (equal '(5d0 5d0 7d0 9d0 9d0)
               (loop for x below 5 collect (uds-ref dset x))))
    (uds-rollback! dset)
    ;; state 1b
    (is (= (uds-root dset 0) (uds-root dset 1)))
    (is (not (uds-connected-p dset 3 4)))
    (is (equal '(2 2 1 1 1)
               (loop for x below 5 collect (uds-size dset x))))
    (is (equal '(5d0 5d0 7d0 3d0 9d0)
               (loop for x below 5 collect (uds-ref dset x))))
    ;; undo empty record
    (signals error (uds-undo! dset))
    (is (null (uds-undo! dset nil))))
  (finishes (make-undoable-disjoint-set 0))
  (finishes (make-undoable-disjoint-set 1)))
