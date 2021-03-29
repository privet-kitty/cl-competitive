(defpackage :cp/persistent-starry-sky-tree
  (:use :cl)
  (:export #:make-psstree #:psstree-fold #:psstree-update-range)
  (:documentation "Provides persistent Starry Sky tree, which is a name given in
the Japanese community to a certain type of restricted lazy segment tree."))
(in-package :cp/persistent-starry-sky-tree)

;; TODO:
;; - test
;; - linear-time initialization

;; supposed to be min or max
(defconstant +op-identity+ 0)
(defun op (x y)
  (declare (fixnum x y))
  (max x y))

(defconstant +updater-identity+ 0)
(defun updater-op (x y)
  (declare (fixnum x y))
  (+ x y))

(deftype index () '(integer 0 #.most-positive-fixnum))

(declaim (inline %power-of-two-ceiling))
(defun %power-of-two-ceiling (x)
  (ash 1 (integer-length (- x 1))))

(declaim (inline make-node))
(defstruct (node (:constructor make-node
                     (&optional (value +op-identity+) (add +updater-identity+))))
  (value nil :type fixnum)
  (add nil :type fixnum)
  (left nil :type (or null node))
  (right nil :type (or null node)))

(defstruct (psstree (:constructor %make-psstree)
                    (:conc-name %psstree-)
                    (:predicate nil))
  (length 0 :type index)
  (root nil :type node))

(defun make-psstree (length)
  (declare (optimize (speed 3))
           (index length))
  (let ((n2 (%power-of-two-ceiling length)))
    (labels ((recur (i)
               (declare (index i))
               (when (<= i n2)
                 (let ((node (make-node)))
                   (setf (node-left node) (recur (ash i 1))
                         (node-right node) (recur (ash i 1)))
                   node))))
      (%make-psstree :length length :root (recur 1)))))

(defun psstree-fold (psstree left right)
  "Folds the given interval [LEFT, RIGHT)."
  (declare (optimize (speed 3))
           (index left right))
  (assert (<= left right (%psstree-length psstree)))
  (labels ((recur (root l r)
             (declare (index l r)
                      (values fixnum &optional))
             (cond ((or (<= right l) (<= r left))
                    +op-identity+)
                   ((and (<= left l) (<= r right))
                    (updater-op (node-value root) (node-add root)))
                   (t
                    (updater-op (op (recur (node-left root) l (ash (+ l r) -1))
                                    (recur (node-right root) (ash (+ l r) -1) r))
                                (node-add root))))))
    (recur (%psstree-root psstree)
           0
           (%power-of-two-ceiling (%psstree-length psstree)))))

(defun psstree-update-range (psstree left right operand)
  "Returns a new PSSTREE updated by PSSTREE[i] := (UPDATER-OP PSSTREE[i]
OPERAND). This function is non-destructive."
  (declare (optimize (speed 3))
           (index left right)
           (fixnum operand))
  (assert (<= left right (%psstree-length psstree)))
  (labels ((recur (root l r)
             (declare (index l r))
             (cond ((or (<= right l) (<= r left)))
                   ((and (<= left l) (<= r right))
                    (setf (node-add root)
                          (updater-op (node-add root) operand)))
                   (t
                    (let ((new-lnode (copy-node (node-left root)))
                          (new-rnode (copy-node (node-right root))))
                      (setf (node-left root) new-lnode
                            (node-right root) new-rnode)
                      (recur new-lnode l (ash (+ l r) -1))
                      (recur new-rnode (ash (+ l r) -1) r)
                      (setf (node-value root)
                            (op (updater-op (node-value new-lnode) (node-add new-lnode))
                                (updater-op (node-value new-rnode) (node-add new-rnode)))))))))
    (let ((new-psstree (copy-psstree psstree))
          (new-root (copy-node (%psstree-root psstree))))
      (recur new-root 0 (%power-of-two-ceiling (%psstree-length psstree)))
      (setf (%psstree-root new-psstree) new-root)
      new-psstree)))

(defmethod print-object ((object psstree) stream)
  (print-unreadable-object (object stream :type t)
    (let ((init t)
          (length (%psstree-length object)))
      (labels ((recur (node index add)
                 (if (node-left node)
                     (let ((new-add (updater-op add (node-add node))))
                       (recur (node-left node) (ash index 1) new-add)
                       (recur (node-right node) (+ (ash index 1) 1) new-add))
                     (when (< index length)
                       (if init
                           (setq init nil)
                           (write-char #\  stream))
                       (write (updater-op (updater-op (node-value node)
                                                      (node-add node))
                                          add)
                              :stream stream)))))
        (recur (%psstree-root object) 0 +updater-identity+)))))
