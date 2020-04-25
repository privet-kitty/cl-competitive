;;;
;;; Persistent segment tree
;;;

;; TODO:
;; - abstraction
;; - test
;; - linear-time initialization
;; - avoid sb-int:power-of-two-ceiling
;; - out-of-bound error

(declaim (inline make-node))
(defstruct (node (:constructor make-node (&optional (value 0))))
  (value 0 :type fixnum)
  (left nil :type (or null node))
  (right nil :type (or null node)))

(defstruct (psegtree (:constructor %make-psegtree)
                     (:conc-name %psegtree-))
  (length 0 :type (integer 0 #.most-positive-fixnum))
  (root nil :type node))

(defun make-psegtree (length)
  "Note that the actual length becomes a power of two."
  (declare ((integer 0 #.most-positive-fixnum) length))
  (let ((n (ash 1 (integer-length (- length 1))))) ; power of two ceiling
    (labels ((recur (i)
               (declare ((integer 0 #.most-positive-fixnum) i))
               (when (<= i n)
                 (let ((node (make-node)))
                   (setf (node-left node) (recur (ash i 1))
                         (node-right node) (recur (ash i 1)))
                   node))))
      (%make-psegtree :length length :root (recur 1)))))

(defun psegtree-query (psegtree left right)
  "Queries the sum of the interval [LEFT, RIGHT)."
  (declare ((integer 0 #.most-positive-fixnum) left right))
  (labels ((recur (root l r)
             (declare ((integer 0 #.most-positive-fixnum) l r)
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
           (sb-int:power-of-two-ceiling (%psegtree-length psegtree)))))

(defun psegtree-update (psegtree index delta)
  "Returns a new psegtree updated by PSEGTREE[INDEX] += DELTA. This function is
non-destructive."
  (declare ((integer 0 #.most-positive-fixnum) index)
           (fixnum delta))
  (labels ((recur (root l r)
             (declare ((integer 0 #.most-positive-fixnum) l r))
             (cond ((or (<= (+ index 1) l) (<= r index)))
                   ((and (<= index l) (<= r (+ index 1)))
                    (incf (node-value root) delta))
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
      (recur new-root 0 (sb-int:power-of-two-ceiling (%psegtree-length psegtree)))
      (setf (%psegtree-root new-psegtree) new-root)
      new-psegtree)))

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
