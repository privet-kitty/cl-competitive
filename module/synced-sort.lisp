(defpackage :cp/synced-sort
  (:use :cl)
  (:export #:synced-sort!))
(in-package :cp/synced-sort)

(declaim (inline %median3))
(defun %median3 (x y z order)
  (if (funcall order x y)
      (if (funcall order y z)
          y
          (if (funcall order z x)
              x
              z))
      (if (funcall order z y)
          y
          (if (funcall order x z)
              x
              z))))

(defun synced-sort! (vector order &rest vectors)
  "Destructively sorts VECTOR by ORDER and applies the same permutation to all
the vectors in VECTORS.

- Not randomized; shuffle the inputs if necessary.
- The consequence is undefined when two or more identical vectors (w.r.t. EQ)
  are given."
  (declare (vector vector))
  (labels
      ((recur (left right)
         (when (< left right)
           (let* ((l left)
                  (r right)
                  (pivot (%median3 (aref vector l)
                                   (aref vector (ash (+ l r) -1))
                                   (aref vector r)
                                   order)))
             (declare ((integer 0 #.most-positive-fixnum) l r))
             (loop (loop while (funcall order (aref vector l) pivot)
                         do (incf l 1))
                   (loop while (funcall order pivot (aref vector r))
                         do (decf r 1))
                   (when (>= l r)
                     (return))
                   (rotatef (aref vector l) (aref vector r))
                   (dolist (v vectors)
                     (rotatef (aref v l) (aref v r)))
                   (incf l 1)
                   (decf r 1))
             (recur left (- l 1))
             (recur (+ r 1) right)))))
    (recur 0 (- (length vector) 1))
    vector))

#+sbcl
(eval-when (:compile-toplevel :load-toplevel :execute)
  (locally (declare (sb-ext:muffle-conditions warning))
    (sb-c:define-source-transform synced-sort! (vector order &rest vectors)
      (let ((vec (gensym))
            (vecs (loop for _ in vectors collect (gensym))))
        `(let ((,vec ,vector)
               ,@(loop for v in vectors
                       for sym in vecs
                       collect `(,sym ,v)))
           (labels
               ((recur (left right)
                  (when (< left right)
                    (let* ((l left)
                           (r right)
                           (pivot (%median3 (aref ,vec l)
                                            (aref ,vec (ash (+ l r) -1))
                                            (aref ,vec r)
                                            ,order)))
                      (declare ((integer 0 #.most-positive-fixnum) l r))
                      (loop (loop while (funcall ,order (aref ,vec l) pivot)
                                  do (incf l 1))
                            (loop while (funcall ,order pivot (aref ,vec r))
                                  do (decf r 1))
                            (when (>= l r)
                              (return))
                            (rotatef (aref ,vec l) (aref ,vec r))
                            ,@(loop for sym in vecs
                                    collect `(rotatef (aref ,sym l) (aref ,sym r)))
                            (incf l 1)
                            (decf r 1))
                      (recur left (- l 1))
                      (recur (+ r 1) right)))))
             (recur 0 (- (length ,vec) 1))
             ,vec))))))
