;;;
;;; MST (Prim's algorithm)
;;; (Poorly implemented and obsolete. I leave it just for my reference.)
;;;

(defpackage :cp/prim
  (:use :cl :cp/abstract-heap)
  (:export #:find-mst))
(in-package :cp/prim)

(define-binary-heap heap
  :order (lambda (x y) (< (the fixnum (cdr x)) (the fixnum (cdr y))))
  :element-type list)

(defun find-mst (graph)
  (let* ((n (length graph))
         (pqueue (make-heap n))
         (added (make-array n :element-type 'bit :initial-element 0))
         (total-cost 0))
    (declare (fixnum total-cost))
    (heap-push (cons 0 0) pqueue)
    (loop until (heap-empty-p pqueue)
          for (current . cost) of-type (fixnum . fixnum) = (heap-pop pqueue)
          when (zerop (aref added current))
          do (setf (aref added current) 1)
             (incf total-cost cost)
             (loop for edge in (aref graph current)
                   when (zerop (aref added (car edge)))
                   do (heap-push (print edge) pqueue)))
    total-cost))

;; Test
(let ((graph (make-array 7 :element-type 'list :initial-element nil)))
  (labels ((add (x y cost)
             (push (cons x cost) (aref graph y))
             (push (cons y cost) (aref graph x))))
    (add 0 1 7)
    (add 0 3 5)
    (add 1 3 9)
    (add 1 2 8)
    (add 2 4 5)
    (add 1 4 7)
    (add 3 4 15)
    (add 3 5 6)
    (add 4 5 8)
    (add 4 6 9)
    (add 5 6 11)
    (assert (= 39 (find-mst graph)))))
