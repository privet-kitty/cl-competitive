(defpackage :cp/test/functional-graph
  (:use :cl :fiveam :cp/functional-graph :cp/scc)
  (:import-from :cp/test/base #:base-suite))
(in-package :cp/test/functional-graph)
(in-suite base-suite)

(test make-cycle-info/hand
  (multiple-value-bind (cycle lengths)
      (make-cycle-info #(1 2 3 4 5 6 3 8 1 10 11 10 6 12 15 14))
    (is (equalp #(3 3 3 3 4 5 6 3 3 10 10 11 6 6 14 15) cycle))
    (is (equalp #(3 2 1 4 4 4 4 4 3 1 2 2 1 2 2 2) lengths)))
  ;; self-loop
  (multiple-value-bind (cycle lengths) (make-cycle-info #(1 2 2))
    (is (equalp #(2 2 2) cycle))
    (is (equalp #(2 1 1) lengths)))
  ;; empty graph
  (multiple-value-bind (cycle lengths) (make-cycle-info #())
    (is (equalp #() cycle))
    (is (equalp #() lengths))))

(test make-cycle-info/random
  (let ((*test-dribble* nil)
        (*random-state* (sb-ext:seed-random-state 0)))
    (dotimes (n 100)
      (let ((graph (make-array n :element-type 'list))
            (fs (make-array n :element-type 'fixnum)))
        (dotimes (_ 5)
          (dotimes (i n)
            (let ((f (aref fs i)))
              (setf (aref fs i) f
                    (aref graph i) (list f))))
          (let* ((scc (make-scc graph))
                 (comps (scc-components scc))
                 (sizes (scc-sizes scc)))
            (multiple-value-bind (cycles lengths) (make-cycle-info fs)
              (dotimes (i n)
                (let ((comp (aref comps i)))
                  (if (= i (aref cycles i))
                      (is (= (aref lengths i) (aref sizes comp)))
                      (is (= 1 (aref sizes comp)))))))))))))
