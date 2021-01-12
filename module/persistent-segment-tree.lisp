(defpackage :cp/persistent-segment-tree
  (:use :cl)
  (:export #:psegtree #:make-psegtree #:psegtree-fold #:psegtree-update #:%psegtree-update!)
  (:documentation "Provides persistent segment tree."))
(in-package :cp/persistent-segment-tree)

;; TODO:
;; - abstraction
;; - test
;; - linear-time initialization

(deftype index () '(integer 0 #.(floor most-positive-fixnum 2)))

(declaim (inline %power-of-two-ceiling))
(defun %power-of-two-ceiling (x)
  (ash 1 (integer-length (- x 1))))

(declaim (inline make-node))
(defstruct (node (:constructor make-node (&optional (value 0))))
  (value 0 :type fixnum)
  (left nil :type (or null node))
  (right nil :type (or null node)))

(defstruct (psegtree (:constructor %make-psegtree)
                     (:conc-name %psegtree-)
                     (:predicate nil))
  (length 0 :type (integer 0 #.most-positive-fixnum))
  (root nil :type node))

(defun make-psegtree (length)
  (declare (index length))
  (let ((n (%power-of-two-ceiling length)))
    (labels ((recur (i)
               (declare (index i))
               (when (<= i n)
                 (let ((node (make-node)))
                   (setf (node-left node) (recur (ash i 1))
                         (node-right node) (recur (ash i 1)))
                   node))))
      (%make-psegtree :length length :root (recur 1)))))

(defun psegtree-fold (psegtree left right)
  "Queries the sum of the interval [LEFT, RIGHT)."
  (declare (optimize (speed 3))
           (index left right))
  (assert (<= left right (%psegtree-length psegtree)))
  (labels ((recur (root l r)
             (declare (index l r)
                      (values fixnum &optional))
             (cond ((or (<= right l) (<= r left))
                    0)
                   ((and (<= left l) (<= r right))
                    (node-value root))
                   (t
                    (+ (recur (node-left root) l (ash (+ l r) -1))
                       (recur (node-right root) (ash (+ l r) -1) r))))))
    (recur (%psegtree-root psegtree)
           0
           (%power-of-two-ceiling (%psegtree-length psegtree)))))

(defun psegtree-update (psegtree index updater)
  "Returns a new psegtree updated by PSEGTREE[INDEX] = (FUNCALL UPDATER
PSEGTREE[INDEX]). This function is non-destructive."
  (declare (optimize (speed 3))
           (index index)
           (function updater))
  (assert (< index (%psegtree-length psegtree)))
  (labels ((recur (root l r)
             (declare (index l r))
             (cond ((or (< index l) (<= r index)))
                   ((= (- r l) 1)
                    (setf (node-value root)
                          (funcall updater (node-value root))))
                   (t
                    (let ((new-lnode (copy-node (node-left root)))
                          (new-rnode (copy-node (node-right root))))
                      (setf (node-left root) new-lnode
                            (node-right root) new-rnode)
                      (recur new-lnode l (ash (+ l r) -1))
                      (recur new-rnode (ash (+ l r) -1) r)
                      (setf (node-value root)
                            (+ (node-value (node-left root))
                               (node-value (node-right root)))))))))
    (let ((new-psegtree (copy-psegtree psegtree))
          (new-root (copy-node (%psegtree-root psegtree))))
      (recur new-root 0 (%power-of-two-ceiling (%psegtree-length psegtree)))
      (setf (%psegtree-root new-psegtree) new-root)
      new-psegtree)))

(defun %psegtree-update! (psegtree index updater)
  "Destructively update PSEGTREE by PSEGTREE[INDEX] = (FUNCALL UPDATER
PSEGTREE[INDEX]).

NOTE: Almost always you should use PSEGTREE-UPDATE. Pay close attention when you
use this destructive version."
  (declare (optimize (speed 3))
           (index index)
           (function updater))
  (assert (< index (%psegtree-length psegtree)))
  (labels ((recur (root l r)
             (declare (index l r))
             (cond ((or (< index l) (<= r index)))
                   ((= (- r l) 1)
                    (setf (node-value root)
                          (funcall updater (node-value root))))
                   (t
                    (recur (node-left root) l (ash (+ l r) -1))
                    (recur (node-right root) (ash (+ l r) -1) r)
                    (setf (node-value root)
                          (+ (node-value (node-left root))
                             (node-value (node-right root))))))))
    (recur (%psegtree-root psegtree)
           0
           (%power-of-two-ceiling (%psegtree-length psegtree)))
    psegtree))

(defmethod print-object ((object psegtree) stream)
  (print-unreadable-object (object stream :type t)
    (let ((init t)
          (length (%psegtree-length object)))
      (labels ((recur (node index)
                 (if (node-left node)
                     (progn
                       (recur (node-left node) (ash index 1))
                       (recur (node-right node) (+ (ash index 1) 1)))
                     (when (< index length)
                       (if init
                           (setq init nil)
                           (write-char #\  stream))
                       (write (node-value node) :stream stream)))))
        (recur (%psegtree-root object) 0)))))
