(eval-when (:compile-toplevel :load-toplevel :execute)
  (load "test-util")
  (load "../explicit-treap.lisp"))

(use-package :test-util)

(defun treap-priority (treap)
  (declare ((or null treap) treap))
  (if (null treap)
      0
      (%treap-priority treap)))

(defun treap-sane-p (treap)
  (or (null treap)
      (and (>= (%treap-priority treap)
               (treap-priority (%treap-left treap)))
           (>= (%treap-priority treap)
               (treap-priority (%treap-right treap)))
           (= (%treap-count treap)
              (+ 1
                 (treap-count (%treap-left treap))
                 (treap-count (%treap-right treap))))
           (= (%treap-accumulator treap)
              (+ (%treap-value treap)
                 (treap-accumulator (%treap-left treap))
                 (treap-accumulator (%treap-right treap))))
           (treap-sane-p (%treap-left treap))
           (treap-sane-p (%treap-right treap)))))

(with-test (:name explicit-treap-sanity)
  (loop repeat 10
        do (assert (treap-sane-p (make-treap #(1 2 3 4 5 6 7 8 9 10))))
           (assert (treap-sane-p (make-treap #(1 2 3 4 5 6 7 8 9))))
           (assert (treap-sane-p (make-treap #(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17))))
           (assert (treap-sane-p (make-treap #(1 2 3 4))))
           (assert (treap-sane-p (make-treap #(1))))
           (assert (treap-sane-p nil))))
