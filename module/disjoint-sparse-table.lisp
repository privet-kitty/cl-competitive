;;;
;;; Disjoint sparse table on arbitrary semigroup
;;;

;;; Reference:
;;; https://discuss.codechef.com/questions/117696/tutorial-disjoint-sparse-table
;;; http://noshi91.hatenablog.com/entry/2018/05/08/183946 (Japanese)
;;; http://drken1215.hatenablog.com/entry/2018/09/08/162600 (Japanese)

(defpackage :cp/disjoint-sparse-table
  (:use :cl)
  (:export #:make-disjoint-sparse-table #:dst-query))
(in-package :cp/disjoint-sparse-table)

;; NOTE: This constructor is slow on SBCL version earlier than 1.5.6 as the type
;; propagation of MAKE-ARRAY doesn't work. The following files are required to
;; enable the optimization.
;; version < 1.5.0: array-element-type.lisp, make-array-header.lisp
;; version < 1.5.6: make-array-header.lisp
(declaim (inline make-disjoint-sparse-table))
(defun make-disjoint-sparse-table (vector binop)
  "BINOP := binary operator (comprising a semigroup)"
  (let* ((n (length vector))
         (height (max 1 (integer-length (- n 1))))
         (table (make-array (list height n) :element-type (array-element-type vector))))
    (dotimes (j n)
      (setf (aref table 0 j) (aref vector j)))
    (do ((i 1 (+ i 1)))
        ((>= i height))
      (let* ((width/2 (ash 1 i))
             (width (* width/2 2)))
        (do ((j 0 (+ j width)))
            ((>= j n))
          (let ((mid (min (+ j width/2) n)))
            ;; fill the first half
            (setf (aref table i (- mid 1))
                  (aref vector (- mid 1)))
            (do ((k (- mid 2) (- k 1)))
                ((< k j))
              (setf (aref table i k)
                    (funcall binop (aref vector k) (aref table i (+ k 1)))))
            (when (>= mid n)
              (return))
            ;; fill the second half
            (setf (aref table i mid)
                  (aref vector mid))
            (let ((end (min n (+ mid width/2))))
              (do ((k (+ mid 1) (+ k 1)))
                  ((>= k end))
                (setf (aref table i k)
                      (funcall binop (aref table i (- k 1)) (aref vector k)))))))))
    table))

(declaim (inline dst-query))
(defun dst-query (table binop left right &optional identity)
  "Queries the interval [LEFT, RIGHT). Returns IDENTITY for a null interval [x,
x)."
  (declare ((integer 0 #.most-positive-fixnum) left right)
           ((simple-array * (* *)) table))
  (when (>= left right)
    (assert (= left right))
    (return-from dst-query identity))
  (setq right (- right 1)) ;; change to closed interval
  (if (= left right)
      (aref table 0 left)
      (let ((h (- (integer-length (logxor left right)) 1)))
        (funcall binop
                 (aref table h left)
                 (aref table h right)))))
