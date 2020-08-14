;;;
;;; Multiset structure by Trie
;;;

(defpackage :cp/trie
  (:use :cl)
  (:export #:trie-char-encode #:trie-node #:make-trie #:%make-trie-node
           #:+trie-alphabet-size+ #:trie-size #:trie-children #:trie-add! #:trie-get))
(in-package :cp/trie)

;; ASCII code:
;; #\A: 65
;; #\a: 97
;; #\0: 48
(declaim (inline trie-char-encode))
(defun trie-char-encode (x)
  (- (char-code x) #.(char-code #\a)))

(defconstant +trie-alphabet-size+ 26)

(declaim (inline %make-trie-node))
(defstruct (trie-node (:constructor %make-trie-node
                          (&aux (children (make-array #.+trie-alphabet-size+
                                                      :element-type t
                                                      :initial-element 0))))
                      (:copier nil)
                      (:predicate nil))
  (size 0 :type (integer 0 #.most-positive-fixnum))
  (children nil :type (simple-array t (#.+trie-alphabet-size+))))

(declaim (inline make-trie))
(defun make-trie () (%make-trie-node))

(declaim (inline trie-add!))
(defun trie-add! (trie-node string)
  "Adds STRING to the trie."
  (declare (vector string))
  (let ((end (length string)))
    (labels ((recur (node position)
               (incf (trie-node-size node))
               (unless (= position end)
                 (let ((children (trie-node-children node))
                       (char (trie-char-encode (aref string position))))
                   (when (eql 0 (aref children char))
                     (setf (aref children char) (%make-trie-node)))
                   (recur (aref children char) (+ 1 position))))))
      (recur trie-node 0)
      trie-node)))

(declaim (inline trie-get))
(defun trie-get (trie-node string &key (start 0) end (target :whole))
  "Returns the number of strings which are registered in TRIE-NODE and

1. whose prefix coincide with STRING, if TARGET is :PREFIX;
2. which coincide with STRING, if TARGET is :WHOLE."
  (declare (vector string)
           ((integer 0 #.most-positive-fixnum) start)
           ((or null (integer 0 #.most-positive-fixnum)) end)
           ((member :prefix :whole) target))
  (let ((end (or end (length string))))
    (labels ((recur (node position)
               (if (= position end)
                   (let ((size (trie-node-size node)))
                     (when (eq target :whole)
                       (loop for child across (trie-node-children node)
                             unless (eql 0 child)
                             do (decf size (trie-node-size child))))
                     size)
                   (let ((children (trie-node-children node))
                         (char (trie-char-encode (aref string position))))
                     (if (eql 0 (aref children char))
                         0
                         (recur (aref children char) (+ 1 position)))))))
      (recur trie-node start))))
