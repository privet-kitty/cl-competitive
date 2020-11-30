(defpackage :cp/rooted-tree-hash
  (:use :cl :cp/integer-hash)
  (:export #:rooted-tree-hash))
(in-package :cp/rooted-tree-hash)

(defconstant +mod+ (- (ash 1 61) 1))
(defconstant +prime+ 10007)

(defun rooted-tree-hash (graph root)
  "Computes hash value of a rooted tree."
  (declare (optimize (speed 3))
           (vector graph)
           ((mod #.array-total-size-limit) root))
  (labels ((recur (v parent)
             (declare ((integer -1 (#.array-total-size-limit)) v parent))
             (let ((value +prime+))
               (declare ((unsigned-byte 61) value))
               (dolist (child (aref graph v))
                 (unless (eql child parent)
                   (setq value
                         (mod (+ value
                                 (ldb (byte 61 0) (integer-hash (recur child v))))
                              +mod+))))
               value)))
    (recur root -1)))
