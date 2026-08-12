(defpackage :cp/test/min-cost-bflow
  (:use :cl :fiveam :cp/min-cost-bflow)
  (:import-from :cp/test/base #:base-suite))
(in-package :cp/test/min-cost-bflow)
(in-suite base-suite)

(test min-cost-bflow/cost
  ;; c anchored at (-1, 4) with slopes -2 (width 3), 1 (width 2): domain
  ;; [-1, 4], vertices (-1, 4), (2, -2), (4, 0).
  (let ((c (make-bflow-cost -1 4 '((-2 . 3) (1 . 2)))))
    (is (= -1 (bflow-cost-lo c)))
    (is (= 4 (bflow-cost-hi c)))
    (is (equal '(4 2 0 -2 -1 0 #.most-positive-fixnum)
               (loop for x from -1 to 5 collect (bflow-cost-value c x))))
    (is (= most-positive-fixnum (bflow-cost-value c -2)))
    ;; Subdifferentials: sentinels at and outside the boundaries, the slope
    ;; pair at the interior kink.
    (is (equal (list most-negative-fixnum most-negative-fixnum)
               (multiple-value-list (bflow-cost-subdiff c -2))))
    (is (equal (list most-negative-fixnum -2)
               (multiple-value-list (bflow-cost-subdiff c -1))))
    (is (equal '(-2 -2) (multiple-value-list (bflow-cost-subdiff c 0))))
    (is (equal '(-2 1) (multiple-value-list (bflow-cost-subdiff c 2))))
    (is (equal (list 1 most-positive-fixnum)
               (multiple-value-list (bflow-cost-subdiff c 4))))
    (is (equal (list most-positive-fixnum most-positive-fixnum)
               (multiple-value-list (bflow-cost-subdiff c 5))))))

(test min-cost-bflow/linear-and-point
  (let ((c (make-linear-bflow-cost 3 -2 2)))
    (is (= -6 (bflow-cost-value-at-lo c)))
    (is (equal '(-6 -3 0 3 6)
               (loop for x from -2 to 2 collect (bflow-cost-value c x))))
    (is (equal '(3 3) (multiple-value-list (bflow-cost-subdiff c 0)))))
  ;; A point domain: value only at LO, all-of-the-line subdifferential.
  (let ((c (make-linear-bflow-cost 5 3 3)))
    (is (null (bflow-cost-segments c)))
    (is (= 3 (bflow-cost-hi c)))
    (is (= 15 (bflow-cost-value c 3)))
    (is (= most-positive-fixnum (bflow-cost-value c 4)))
    (is (equal (list most-negative-fixnum most-positive-fixnum)
               (multiple-value-list (bflow-cost-subdiff c 3))))))

(test min-cost-bflow/validation
  (signals error (make-bflow-cost 0 0 '((1 . 0))))
  (signals error (make-bflow-cost 0 0 '((1 . -2))))
  (signals error (make-bflow-cost 0 0 '((2 . 1) (2 . 1))))
  (signals error (make-bflow-cost 0 0 '((2 . 1) (1 . 1))))
  (signals error (make-linear-bflow-cost 1 2 1))
  (signals error (make-bflow-problem 2 '(1 -1 0) '()))
  (signals error (make-bflow-problem
                  2 '(0 0)
                  (list (make-bflow-arc 0 2 (make-linear-bflow-cost 1 0 1)))))
  (let ((problem (make-bflow-problem
                  2 '(1 -1)
                  (list (make-bflow-arc 0 1 (make-linear-bflow-cost 1 0 1))))))
    (is (= 2 (bflow-problem-num-nodes problem)))
    (is (equalp #(1 -1) (bflow-problem-balances problem)))
    (is (= 1 (length (bflow-problem-arcs problem))))))
