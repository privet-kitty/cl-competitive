(defpackage :cp/parse-parens
  (:use :cl)
  (:export #:make-tree-from-parens))
(in-package :cp/parse-parens)

(declaim (ftype (function * (values (simple-array list (*))
                                    (simple-array fixnum (*))
                                    &optional))
                make-tree-from-parens))
(defun make-tree-from-parens (vector &optional (left-paren #\() (test #'eql))
  "Parses (maybe broken) parentheses, constructs a corresponding forest, and
returns it as a vector of adjacency lists, whose length is length(VECTOR) +
1. The last list of the returned vector consists of the roots of all the
forests. As the second value this function returns the vector of matched
position of each parenthesis, where unmatched parenthesis is represented as -1.

Example:
(cp/parse-parens::make-tree-from-parens \")(()())\")
=> #(NIL (2 4) NIL NIL NIL NIL NIL (1))
#(-1 6 3 2 5 4 1)
"
  (declare (vector vector))
  (let* ((n (length vector))
         (assign (make-array n :element-type 'fixnum :initial-element -1))
         (graph (make-array (+ n 1) :element-type 'list :initial-element nil)))
    (let (stack)
      (dotimes (i n)
        (let ((c (aref vector i)))
          (cond ((funcall test left-paren c)
                 (push i stack))
                (stack
                 (let ((prev (pop stack)))
                   (setf (aref assign prev) i
                         (aref assign i) prev)))))))
    (labels ((recur (pos parent)
               (declare (fixnum pos))
               (loop do (loop while (and (< pos n)
                                         (= -1 (aref assign pos)))
                              do (incf pos))
                     while (and (< pos n) (funcall test left-paren (aref vector pos)))
                     collect pos into list
                     do (setq pos (+ 1 (recur (+ pos 1) pos)))
                     finally (setf (aref graph parent) list)
                             (return pos))))
      (recur 0 n)
      (values graph assign))))
