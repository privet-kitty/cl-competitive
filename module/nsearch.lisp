(defpackage :cp/nsearch
  (:use :cl)
  (:export #:nsearch))
(in-package :cp/nsearch)

;; FIXME: a bit confusing name
;; NOTE: not tested

(declaim (ftype (function * (values list &optional)) nsearch))
(defun nsearch (sub-vector1 main-vector2
                &key (test #'eql) from-end (start1 0) end1 (start2 0) end2)
  "Is a non-contiguous version of CL:SEARCH. Returns a list of first matched
indices.

Example:
\(cp/nsearch::nsearch \"LISP\" \"..L.I.SPLISP\")
-> (2 4 6 7)
\(cp/nsearch::nsearch \"LISP\" \"..L.I.S.PLISP\" :from-end t)
-> (11 10 9 8)
\(cp/nsearch::nsearch \"LISP\" \"MINEAPOLLIS\")
-> NIL
"
  (declare (vector sub-vector1 main-vector2)
           ((mod #.array-dimension-limit) start1 start2)
           ((or null (mod #.array-dimension-limit)) end1 end2))
  (let ((end1 (or end1 (length sub-vector1)))
        (end2 (or end2 (length main-vector2))))
    (if from-end
        (let ((i2 (- end2 1)))
          (declare ((integer -1 (#.array-dimension-limit)) i2))
          (loop for i1 from (- end1 1) downto start1
                do (loop while (and (>= i2 start2)
                                    (not (funcall test
                                                  (aref sub-vector1 i1)
                                                  (aref main-vector2 i2))))
                         do (decf i2))
                   (when (< i2 start2)
                     (return-from nsearch))
                collect i2
                do (decf i2)))
        (let ((i2 start2))
          (declare ((integer 0 (#.array-dimension-limit)) i2))
          (loop for i1 from start1 below end1
                do (loop while (and (< i2 end2)
                                    (not (funcall test
                                                  (aref sub-vector1 i1)
                                                  (aref main-vector2 i2))))
                         do (incf i2))
                   (when (= i2 end2)
                     (return-from nsearch))
                collect i2
                do (incf i2))))))
