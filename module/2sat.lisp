(defpackage :cp/2sat
  (:use :cl :cp/scc)
  (:export #:2sat #:make-2sat #:2sat-p
           #:add-implication #:add-disjunction #:2sat-solve)
  (:documentation
   "Provides 2-SAT solver.

Usage note:
Use LOGNOT for negation.

Example:
P implies Q: (add-implication p q)
(not P) implies Q: (add-implication (lognot p) q)
P or Q: (add-disjunction p q)"))
(in-package :cp/2sat)

(defstruct (2sat (:constructor make-2sat
                     (size
                      &aux
                      (graph (make-array (* 2 size) :element-type 'list :initial-element nil))))
                 (:copier nil))
  (size 0 :type (mod #.array-dimension-limit))
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

(defun add-implication (2sat p q)
  "Adds `P implies Q' to 2SAT."
  (declare (fixnum p q))
  (%add-implication 2sat p q)
  (%add-implication 2sat (lognot q) (lognot p))
  2sat)

(declaim (inline add-disjunction))
(defun add-disjunction (2sat p q)
  "Adds `P or Q' to 2SAT."
  (declare (fixnum p q))
  (%add-implication 2sat (lognot p) q)
  (%add-implication 2sat (lognot q) p)
  2sat)

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

