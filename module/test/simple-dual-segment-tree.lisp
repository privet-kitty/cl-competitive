(defpackage :cp/test/simple-dual-segment-tree
  (:use :cl :fiveam :cp/simple-dual-segment-tree)
  (:import-from :cp/test/base #:base-suite))
(in-package :cp/test/simple-dual-segment-tree)
(in-suite base-suite)

(define-simple-dual-segtree segtree
  :operator #'+
  :identity 0
  :element-type fixnum)

(test simple-dual-segment-tree-+
  (let ((*test-dribble* nil)
        (*random-state* (sb-ext:seed-random-state 0)))
    (dotimes (len 25)
      (let ((vector (make-array len :element-type 'fixnum)))
        (when (zerop (random 2))
          (dotimes (i len)
            (setf (aref vector i) (- (random 10) 5))))
        (let ((segtree (make-segtree len :initial-contents vector)))
          (dotimes (_ 100)
            (let ((l (random (+ len 1)))
                  (r (random (+ len 1)))
                  (operand (- (random 100) 50)))
              (when (> l r)
                (rotatef l r))
              (loop for i from l below r
                    do (incf (aref vector i) operand))
              (segtree-update segtree operand l r)
              (dotimes (i len)
                (is (= (aref vector i) (segtree-ref segtree i)))))))))))

(sb-int:defconstant-eqx +iden+ (cons most-negative-fixnum #\a) #'equal)

(define-simple-dual-segtree chtree
  :operator (lambda (node1 node2)
              (if (> (car node1) (car node2))
                  node1 node2))
  :identity +iden+
  :element-type t)

(defun randc ()
  (code-char (+ #.(char-code #\a) (random 26))))

(test simple-dual-segment-tree-range-fill
  (let ((*test-dribble* nil)
        (*random-state* (sb-ext:seed-random-state 0)))
    (dotimes (len 25)
      (let ((vector (make-array len :element-type t :initial-element +iden+)))
        (when (zerop (random 2))
          (let ((init-c (cons -1 (randc))))
            (dotimes (i len)
              (setf (aref vector i) init-c))))
        (let ((segtree (make-chtree len :initial-contents vector)))
          (dotimes (time 100)
            (let ((l (random (+ len 1)))
                  (r (random (+ len 1)))
                  (operand (cons time (randc))))
              (when (> l r)
                (rotatef l r))
              (loop for i from l below r
                    do (setf (aref vector i) operand))
              (chtree-update segtree operand l r)
              (dotimes (i len)
                (is (eql (aref vector i) (chtree-ref segtree i)))))))))))
