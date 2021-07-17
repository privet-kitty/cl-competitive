(defpackage :cp/2sat
  (:use :cl :cp/scc :cp/extend-vector)
  (:export #:2sat #:make-2sat #:2sat-p
           #:2sat-add-imply #:2sat-add-or #:2sat-add-xor #:2sat-solve)
  (:documentation
   "Provides 2-SAT solver.

Usage note:
Use LOGNOT for negation.

Example:
P implies Q: (add-implication p q)
\(not P) implies Q: (add-implication (lognot p) q)
P or Q: (add-disjunction p q)"))
(in-package :cp/2sat)

(defstruct (2sat (:constructor make-2sat
                     (size
                      &aux
                      (end (* 2 size))
                      (graph (make-array end :element-type 'list :initial-element nil))))
                 (:copier nil)
                 (:predicate nil))
  ;; number of variables
  (size nil :type (mod #.array-dimension-limit))
  ;; current length of used cells
  (end nil :type (mod #.array-dimension-limit))
  (graph nil :type (simple-array list (*)))
  (scc nil :type (or null scc)))

(declaim (inline %add-implication))
(defun %add-implication (2sat p q)
  (declare (fixnum p q))
  (let ((size (2sat-size 2sat))
        (graph (2sat-graph 2sat)))
    (when (< p 0)
      (setq p (+ size (lognot p))))
    (when (< q 0)
      (setq q (+ size (lognot q))))
    (push q (aref graph p))
    2sat))

(declaim (inline 2sat-add-imply))
(defun 2sat-add-imply (2sat p q)
  "Adds `P implies Q' to 2SAT."
  (declare (fixnum p q))
  (%add-implication 2sat p q)
  (%add-implication 2sat (lognot q) (lognot p))
  2sat)

(declaim (inline 2sat-add-or))
(defun 2sat-add-or (2sat p q)
  "Adds `P or Q' to 2SAT."
  (declare (fixnum p q))
  (%add-implication 2sat (lognot p) q)
  (%add-implication 2sat (lognot q) p)
  2sat)

(declaim (inline 2sat-add-xor))
(defun 2sat-add-xor (2sat vector)
  "Adds `one and only one of VECTOR[0], VECTOR[1], ...' to 2SAT."
  (declare (vector vector))
  (let ((len (length vector))
        (end (2sat-end 2sat))
        (size (2sat-size 2sat)))
    (extend-vectorf (2sat-graph 2sat) (+ end (* 2 len)) nil)
    (let ((graph (2sat-graph 2sat)))
      (labels ((%get (v slack-p)
                 (declare (fixnum v))
                 (if (< v 0)
                     (let ((v (lognot v)))
                       (if slack-p
                           (+ end len v)
                           (+ size v)))
                     (if slack-p
                         (+ end v)
                         v)))
               (%push (v1 slack-p1 v2 slack-p2)
                 (push (%get v2 slack-p2) (aref graph (%get v1 slack-p1)))))
        ;; Q[i] := P[0] or P[1] or ... or P[i]
        (dotimes (i len)
          ;; P[i] implies Q[i]
          (%push (aref vector i) nil i t)
          (%push (lognot i) t (lognot (aref vector i)) nil))
        (dotimes (i (- len 1))
          ;; Q[i] implies Q[i+1]
          (%push i t (+ i 1) t)
          (%push (lognot (+ i 1)) t (lognot i) t)
          ;; not (Q[i] and P[i+1])
          (%push i t (lognot (aref vector (+ i 1))) nil)
          (%push (aref vector (+ i 1)) nil (lognot i) t)))
      (incf (2sat-end 2sat) (* 2 len))
      2sat)))

(declaim (inline 2sat-solve))
(defun 2sat-solve (2sat)
  "Solves 2-SAT and returns a simple bit vector expressing the boolean of each
variable if it is feasible, otherwise returns NIL."
  (let* ((size (2sat-size 2sat))
         (graph (2sat-graph 2sat))
         (scc (make-scc graph))
         (components (scc-components scc))
         (result (make-array size :element-type 'bit :initial-element 0)))
    (setf (2sat-scc 2sat) scc)
    (loop for v below size
          for v-comp = (aref components v)
          for neg-comp = (aref components (+ v size))
          do (cond ((> v-comp neg-comp)
                    (setf (sbit result v) 1))
                   ((= v-comp neg-comp)
                    (return-from 2sat-solve nil))))
    result))
