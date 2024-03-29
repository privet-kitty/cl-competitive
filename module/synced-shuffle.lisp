(defpackage :cp/synced-shuffle
  (:use :cl)
  (:export #:synced-shuffle!))
(in-package :cp/synced-shuffle)

(defun synced-shuffle! (vector &rest vectors)
  "Destructively shuffles VECTOR and applies the same permutation to all the
vectors in VECTORS."
  (loop for i from (- (length vector) 1) above 0
        for j = (random (+ i 1))
        do (rotatef (aref vector i) (aref vector j))
           (dolist (v vectors)
             (rotatef (aref v i) (aref v j)))
        finally (return vector)))

#+sbcl
(eval-when (:compile-toplevel :load-toplevel :execute)
  (locally (declare (sb-ext:muffle-conditions warning))
    (sb-c:define-source-transform synced-shuffle! (vector &rest vectors)
      (let ((vec (gensym))
            (vecs (loop for _ in vectors collect (gensym))))
        `(let ((,vec ,vector)
               ,@(loop for v in vectors
                       for sym in vecs
                       collect `(,sym ,v)))
           (loop for i from (- (length ,vec) 1) above 0
                 for j = (random (+ i 1))
                 do (rotatef (aref ,vec i) (aref ,vec j))
                    ,@(loop for sym in vecs
                            collect `(rotatef (aref ,sym i) (aref ,sym j)))
                 finally (return ,vec)))))))
