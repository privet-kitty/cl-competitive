(defpackage :cp/test/boruvka
  (:use :cl :fiveam :cp/boruvka :cp/disjoint-set)
  (:import-from :cp/test/base #:base-suite))
(in-package :cp/test/boruvka)
(in-suite base-suite)

(defun make-random-graph (n density)
  (let ((graph (make-array n :element-type 'list :initial-element nil)))
    (dotimes (u n)
      (loop for v from (+ u 1) below n
            for cost = (random most-positive-fixnum)
            when (<= (random 1d0) density)
            do (push (cons v cost) (aref graph u))
               (push (cons u cost) (aref graph v))))
    graph))

;; Kruskal
(defun find-mst2 (graph maximize)
  (let* (edges
         (n (length graph))
         (dset (make-disjoint-set n))
         (res 0))
    (dotimes (u n)
      (loop for (v . cost) in (aref graph u)
            when (< u v)
            do (push (list cost u v) edges)))
    (setq edges (sort edges (if maximize #'> #'<)
                      :key #'car))
    (loop for (cost u v) in edges
          unless (ds-connected-p dset u v)
          do (incf res cost)
             (ds-unite! dset u v))
    res))

(test boruvka
  ;; empty graph
  (multiple-value-bind (costs e1 e2) (find-mst #())
    (is (equalp #() costs))
    (is (equalp #() e1))
    (is (equalp #() e2)))
  ;; self loop
  (multiple-value-bind (costs e1 e2) (find-mst #(((0 . 10))))
    (is (equalp #() costs))
    (is (equalp #() e1))
    (is (equalp #() e2)))
  ;; minimize random graph
  (finishes
    (dotimes (_ 500)
      (let ((graph (make-random-graph 40 (random 1.0))))
        (assert (= (find-mst2 graph nil) (reduce #'+ (find-mst graph)))))))
  ;; maximize random graph
  (finishes
    (dotimes (_ 500)
      (let ((graph (make-random-graph 40 (random 1.0))))
        (assert (= (find-mst2 graph t) (reduce #'+ (find-mst graph :maximize t))))))))
