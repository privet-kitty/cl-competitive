;;;
;;; Linear-time graph chordality recognition
;;;

(defpackage :cp/chordal-graph
  (:use :cl)
  (:export #:perfect-elimination-order-p #:make-perfect-elimination-order))
(in-package :cp/chordal-graph)

;; Reference:
;; Therese Biedl. Graph-theoretic algorithms. Lecture notes of a graduate course, University of Waterloo
(defun perfect-elimination-order-p (graph order)
  "Returns true iff ORDER is a perfect elimination order of GRAPH.
GRAPH must be undirected.

GRAPH := vector of adjacency lists
ORDER := vector of (0-based) vertex numbers"
  (declare (vector graph order))
  (let* ((n (length graph))
         (invs (make-array n :element-type '(integer 0 #.most-positive-fixnum)))
         (table (make-hash-table :test #'equal)))
    (assert (= n (length order)))
    (dotimes (i n)
      (setf (aref invs (aref order i)) i))
    (loop for i from (- n 1) downto 0
          for new-node = (aref order i)
          for last-pred = nil
          for last-index = most-positive-fixnum
          do (dolist (neighbor (aref graph new-node))
               (when (< i (aref invs neighbor) last-index)
                 (setq last-pred neighbor
                       last-index (aref invs neighbor))))
             (dolist (neighbor (aref graph new-node))
               (when (and (< i (aref invs neighbor))
                          (/= last-pred neighbor))
                 (let ((edge (if (< last-pred neighbor)
                                 (cons last-pred neighbor)
                                 (cons neighbor last-pred))))
                   (setf (gethash edge table) t)))))
    (dotimes (v n)
      (dolist (neighbor (aref graph v))
        (declare ((integer 0 #.most-positive-fixnum) neighbor))
        (let ((edge (if (< v neighbor)
                        (cons v neighbor)
                        (cons neighbor v))))
          (remhash edge table))))
    (zerop (hash-table-count table))))


;; Maximum cardinality search in O(n) time
;; Reference: Blair, Peyton. An introduction to chordal graphs and clique trees

;; TODO: survey the original paper Tarjan, Yannakakis (1984)

(declaim (ftype (function * (values (or null (simple-array (integer 0 #.most-positive-fixnum)))
                                    &optional))
                make-perfect-elimination-order))
(defun make-perfect-elimination-order (graph)
  "Returns a perfect elimination ordering of an undirected graph if it is
chordal, otherwise returns NIL.

Note that this function doesn't check GRAPH is really undirected.

GRAPH := vector of adjacency lists"
  (declare (vector graph))
  (let* ((n (length graph))
         (degrees (make-array n :element-type '(integer 0 #.most-positive-fixnum)
                                :initial-element 0))
         (tops (make-array n :element-type 'fixnum :initial-element -1))
         (deg-prevs (make-array n :element-type 'fixnum :initial-element -1))
         (deg-nexts (make-array n :element-type 'fixnum :initial-element -1))
         (prevs (make-array n :element-type 'fixnum :initial-element -1))
         (nexts (make-array n :element-type 'fixnum :initial-element -1))
         (highest 0)
         (marked (make-array n :element-type 'bit :initial-element 0))
         (res (make-array n :element-type '(integer 0 #.most-positive-fixnum))))
    (declare (fixnum highest))
    (setf (aref tops 0) 0)
    (dotimes (i n)
      (setf (aref prevs i) (- i 1)
            (aref nexts i) (if (= i (- n 1)) -1 (+ i 1))))
    (labels ((%remove (v)
               (let ((degree (aref degrees v)))
                 ;; cut off V from the previous and the next vertex, link them
                 ;; to each other
                 (when (>= (aref prevs v) 0)
                   (setf (aref nexts (aref prevs v)) (aref nexts v)))
                 (when (>= (aref nexts v) 0)
                   (setf (aref prevs (aref nexts v)) (aref prevs v)))
                 ;; when V is at the top of the list, change the top
                 (when (= (aref tops degree) v)
                   (if (= (aref nexts v) -1)
                       ;; if no vertices of DEGREE no longer exist, link
                       ;; DEG-PREVS[DEGREE] and DEG-NEXTS[DEGREE] to each other.
                       (progn
                         ;; update HIGHEST if DEGREE is the current highest.
                         (when (= highest degree)
                           (setq highest (aref deg-prevs degree)))
                         (setf (aref tops degree) -1)
                         (unless (= (aref deg-prevs degree) -1)
                           (setf (aref deg-nexts (aref deg-prevs degree))
                                 (aref deg-nexts degree)))
                         (unless (= (aref deg-nexts degree) -1)
                           (setf (aref deg-prevs (aref deg-nexts degree))
                                 (aref deg-prevs degree)))
                         ;; cut off DEGREE from the previous and the next degree
                         (setf (aref deg-prevs degree) -1
                               (aref deg-nexts degree) -1))
                       (setf (aref tops degree) (aref nexts v))))
                 ;; reinitialize V
                 (setf (aref prevs v) -1
                       (aref nexts v) -1)))
             (%pop ()
               (let ((res (aref tops highest)))
                 (%remove res)
                 (setf (aref marked res) 1)
                 res))
             (%promote (v)
               "Increments degree of V"
               (let* ((degree (aref degrees v))
                      (next-degree (aref deg-nexts degree))
                      (prev-degree (aref deg-prevs degree)))
                 (%remove v)
                 (incf (aref degrees v))
                 ;; push V at the top of the list, degree + 1
                 (let ((top (aref tops (+ degree 1))))
                   (if (= top -1)
                       ;; make a new list when no other vertices have degree + 1
                       (progn
                         (setf (aref deg-nexts degree) (+ degree 1)
                               (aref deg-prevs (+ degree 1)) degree)
                         (when (> next-degree (+ degree 1))
                           (setf (aref deg-nexts (+ degree 1)) next-degree
                                 (aref deg-prevs next-degree) (+ degree 1))))
                       (setf (aref prevs top) v
                             (aref nexts v) top
                             (aref prevs v) -1))
                   ;; reduce PREV-DEGREE when vertices of this degree no longer exist
                   (when (and (/= prev-degree -1)
                              (= (aref tops degree) -1))
                     (setf (aref deg-prevs (+ degree 1)) prev-degree
                           (aref deg-nexts prev-degree) (+ degree 1)))
                   (setf (aref tops (+ degree 1)) v))
                 ;; update HIGHEST
                 (setq highest (max highest (+ degree 1))))))
      (loop for i from (- n 1) downto 0
            for v = (%pop)
            do (setf (aref res i) v)
               (dolist (neighbor (aref graph v))
                 (when (zerop (aref marked neighbor))
                   (%promote neighbor))))
      (and (perfect-elimination-order-p graph res) res))))
