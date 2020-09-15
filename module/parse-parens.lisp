(defpackage :cp/parse-parens
  (:use :cl)
  (:export #:parse-parens))
(in-package :cp/parse-parens)

(declaim (ftype (function * (values (simple-array list (*)) list &optional))
                make-forest-from-parens))
(defun make-forest-from-parens (vector &optional (left-paren #\() (test #'eql))
  "Parses parentheses and constructs a corresponding forest. Returns two values:
vector of adjacency lists and list of root vertices."
  (declare (vector vector)
           (function test))
  (let* ((n (length vector))
         (graph (make-array n :element-type 'list :initial-element nil))
         roots)
    (labels ((recur (pos parent)
               (declare (fixnum pos))
               (loop while (and (< pos n) (funcall test left-paren (aref vector pos)))
                     collect pos into list
                     do (setq pos (+ 1 (recur (+ pos 1) pos)))
                     finally (if (= parent -1)
                                 (setq roots list)
                                 (setf (aref graph parent) list))
                             (return pos))))
      (recur 0 -1)
      (values graph roots))))
