(defpackage :cp/tree-hash
  (:use :cl :cp/integer-hash)
  (:export #:tree-root-hash #:tree-all-hashes))
(in-package :cp/tree-hash)

;; Reference:
;; https://snuke.hatenablog.com/entry/2017/02/03/054210 (Japanese)

(defconstant +mod+ (- (ash 1 61) 1))
(defconstant +prime+ 10007)

(declaim (inline %integer-hash61))
(defun %integer-hash61 (x)
  (ldb (byte 61 0) (integer-hash x)))

(defun tree-root-hash (graph root)
  "Computes hash value of a rooted tree."
  (declare (optimize (speed 3))
           (vector graph)
           ((mod #.array-dimension-limit) root))
  (labels ((recur (v parent)
             (declare ((integer -1 (#.array-dimension-limit)) v parent))
             (let ((value +prime+))
               (declare ((unsigned-byte 61) value))
               (dolist (child (aref graph v))
                 (unless (eql child parent)
                   (setq value
                         (mod (+ value (%integer-hash61 (recur child v)))
                              +mod+))))
               value)))
    (recur root -1)))

(defun tree-all-hashes (graph)
  "Computes hash values for all roots of a given tree in linear time."
  (declare (optimize (speed 3))
           (vector graph))
  (let* ((n (length graph))
         (dp (make-array n :element-type '(unsigned-byte 62) :initial-element 0))
         (res (make-array n :element-type '(unsigned-byte 62) :initial-element 0)))
    (labels ((dfs1 (v parent)
               (declare ((integer -1 (#.array-dimension-limit)) v parent))
               (let ((value +prime+))
                 (declare ((unsigned-byte 61) value))
                 (dolist (child (aref graph v))
                   (unless (eql child parent)
                     (setq value
                           (mod (+ value (%integer-hash61 (dfs1 child v)))
                                +mod+))))
                 (setf (aref dp v) value)))
             (dfs2 (v parent)
               (declare ((integer -1 (#.array-dimension-limit)) v parent))
               (setf (aref res v) (aref dp v))
               (dolist (child (aref graph v))
                 (unless (eql child parent)
                   (let* ((old-v-value (aref dp v))
                          (old-child-value (aref dp child))
                          (new-v-value (let ((tmp (- old-v-value
                                                     (%integer-hash61 old-child-value))))
                                         (if (>= tmp 0) tmp (+ tmp +mod+))))
                          (new-child-value (mod (+ old-child-value
                                                   (%integer-hash61 new-v-value))
                                                +mod+)))
                     (declare ((unsigned-byte 61)
                               old-v-value old-child-value
                               new-v-value new-child-value))
                     (setf (aref dp v) new-v-value
                           (aref dp child) new-child-value)
                     (dfs2 child v)
                     (setf (aref dp v) old-v-value
                           (aref dp child) old-child-value))))))
      (dfs1 0 -1)
      (dfs2 0 -1)
      res)))
