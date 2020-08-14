;;;
;;; Minimum spanning tree (Boruvka's algorithm, O(ElogV))
;;;
;;; Reference:
;;; Ahuja, Magnanti, Orlin. Network Flows: Theory, Algorithms, and Applications.
;;;

(defpackage :cp/boruvka
  (:use :cl)
  (:export #:find-mst))
(in-package :cp/boruvka)

(defconstant +inf-cost+ most-positive-fixnum)

;; Here I express each connected component as a cycle with singly-linked list,
;; though Ahuja's book adopts doubly-linked list. I learned about this technique
;; in noshi91's article: http://noshi91.hatenablog.com/entry/2019/07/19/180606
;; (Japanese)

(declaim (inline find-mst)
         (ftype (function * (values (simple-array fixnum (*))
                                    (simple-array fixnum (*))
                                    (simple-array fixnum (*))
                                    &optional))
                find-mst))
(defun find-mst (graph &key (vertex-key #'car) (cost-key #'cdr) maximize)
  "Computes an MST by Boruvka's algorithm. Returns three values: a vector that
stores each cost of the edges, two vectors that store each end of the edges. If
GRAPH is not connected, this function computes MST for each connected component.

GRAPH := vector of adjacency lists
MAXIMIZE := if true, solve maximization problem instead"
  (declare (vector graph))
  (let* ((n (length graph))
         (roots (make-array n :element-type 'fixnum))
         ;; next node in a connected component
         (nexts (make-array n :element-type 'fixnum))
         (min-costs (make-array n :element-type 'fixnum))
         (min-srcs (make-array n :element-type 'fixnum))
         (min-dests (make-array n :element-type 'fixnum))
         (edge-count 0)
         (res-costs (make-array (max 0 (- n 1)) :element-type 'fixnum))
         (res-srcs (make-array (max 0 (- n 1)) :element-type 'fixnum))
         (res-dests (make-array (max 0 (- n 1)) :element-type 'fixnum)))
    (dotimes (i n)
      (setf (aref roots i) i
            (aref nexts i) i))
    (labels ((%add (src dest cost)
               "Adds a new edge to the (unfinished) MST."
               (declare (fixnum src dest cost))
               (when (> src dest) (rotatef src dest))
               (setf (aref res-srcs edge-count) src
                     (aref res-dests edge-count) dest
                     (aref res-costs edge-count) (if maximize (- cost) cost)
                     edge-count (+ edge-count 1)))
             (%merge (root1 root2)
               "Merges ROOT2 into ROOT1."
               (loop for v = (aref nexts root2) then (aref nexts v)
                     until (= v root2)
                     do (setf (aref roots v) root1)
                     finally (setf (aref roots v) root1))
               ;; meld two cycles
               (rotatef (aref nexts root1) (aref nexts root2))))
      (loop for updated = nil
            while (< edge-count (- n 1))
            do (fill min-costs +inf-cost+)
               ;; detect minimum cost edge starting from each connected component
               (dotimes (u n)
                 (let ((root (aref roots u)))
                   (dolist (edge (aref graph u))
                     (let ((v (funcall vertex-key edge))
                           (cost (funcall cost-key edge)))
                       (declare (fixnum v cost))
                       (when maximize (setq cost (- cost)))
                       (when (and (/= root (aref roots v))
                                  (<= cost (aref min-costs root)))
                         (setf (aref min-costs root) cost
                               (aref min-srcs root) u
                               (aref min-dests root) v))))))
               ;; merge all the pairs of connected components linked with above
               ;; enumerated edges
               (dotimes (v n)
                 (let* ((root (aref roots v))
                        (src (aref min-srcs root))
                        (dest (aref min-dests root))
                        (cost (aref min-costs root)))
                   (unless (= cost +inf-cost+) ; can be true if GRAPH is not connected
                     (loop for root2 = (aref roots dest)
                           until (= root root2)
                           do (setq updated t)
                              (%add src dest cost)
                              (%merge root root2)
                              (setq src (aref min-srcs root2)
                                    dest (aref min-dests root2)
                                    cost (aref min-costs root2))))))
            while updated)
      (values (adjust-array res-costs edge-count)
              (adjust-array res-srcs edge-count)
              (adjust-array res-dests edge-count)))))
