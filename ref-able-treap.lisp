;; Treap accessible by index (O(log(n))).
;; Virtually it works like std::set of C++ or TreeSet of Java. 

;; Note:
;; - You shouldn't insert duplicate keys into a treap unless you know what you
;; are doing.
;; - You cannot rely on the side effect when you call any destructive operations
;; on a treap. Always use the returned value.
;; - An empty treap is NIL.

(defstruct (treap (:constructor %make-treap (key priority &key left right (count 1)))
                  (:copier nil)
                  (:conc-name %treap-))
  key
  (priority 0 :type (integer 0 #.most-positive-fixnum))
  (count 0 :type (integer 0 #.most-positive-fixnum))
  (left nil :type (or null treap))
  (right nil :type (or null treap)))

(declaim (inline treap-count))
(defun treap-count (treap)
  "Returns the size of the (nullable) TREAP."
  (declare ((or null treap) treap))
  (if (null treap)
      0
      (%treap-count treap)))

(declaim (inline update-count))
(defun update-count (treap)
  (declare (treap treap))
  (setf (%treap-count treap)
        (+ 1
           (treap-count (%treap-left treap))
           (treap-count (%treap-right treap)))))

(declaim (inline treap-find))
(defun treap-find (key treap &key (order #'<))
  "Returns KEY if TREAP contains it, otherwise NIL.

An element in TREAP is considered to be equal to KEY iff (and (not (funcall
order key <element>)) (not (funcall order <element> key))) is true."
  (declare ((or null treap) treap))
  (labels ((recur (treap)
             (cond ((null treap) nil)
                   ((funcall order key (%treap-key treap))
                    (recur (%treap-left treap)))
                   ((funcall order (%treap-key treap) key)
                    (recur (%treap-right treap)))
                   (t key))))
    (recur treap)))

(declaim (inline treap-position))
(defun treap-position (key treap &key (order #'<))
  "Returns the index if TREAP contains KEY, otherwise NIL.

An element in TREAP is considered to be equal to KEY iff (and (not (funcall
order key <element>)) (not (funcall order <element> key))) is true."
  (declare ((or null treap) treap))
  (labels ((recur (count treap)
             (declare ((integer 0 #.most-positive-fixnum) count))
             (cond ((null treap) nil)
                   ((funcall order (%treap-key treap) key)
                    (recur count (%treap-right treap)))
                   ((funcall order key (%treap-key treap))
                    (let ((left-count (- count (treap-count (%treap-right treap)) 1)))
                      (recur left-count (%treap-left treap))))
                   (t (- count (treap-count (%treap-right treap)) 1)))))
    (recur (treap-count treap) treap)))

(declaim (inline treap-bisect-left)
         (ftype (function * (values (integer 0 #.most-positive-fixnum) t &optional)) treap-bisect-left))
(defun treap-bisect-left (value treap &key (order #'<))
  "Returns the smallest index and the corresponding key that satisfies
TREAP[index] >= VALUE. Returns the size of TREAP and VALUE if TREAP[size-1] <
VALUE."
  (labels ((recur (count treap)
             (declare ((integer 0 #.most-positive-fixnum) count))
             (cond ((null treap) (values nil nil))
                   ((funcall order (%treap-key treap) value)
                    (recur count (%treap-right treap)))
                   (t (let ((left-count (- count (treap-count (%treap-right treap)) 1)))
                        (multiple-value-bind (idx key)
                            (recur left-count (%treap-left treap))
                          (if idx
                              (values idx key)
                              (values left-count (%treap-key treap)))))))))
    (declare (ftype (function * (values t t &optional)) recur))
    (multiple-value-bind (idx key)
        (recur (treap-count treap) treap)
      (if idx
          (values idx key)
          (values (treap-count treap) value)))))

(declaim (inline treap-split)
         (ftype (function * (values (or null treap) (or null treap) &optional)) treap-split))
(defun treap-split (key treap &key (order #'<))
  "Destructively splits the TREAP with reference to KEY and returns two treaps,
the smaller sub-treap (< KEY) and the larger one (>= KEY)."
  (declare ((or null treap) treap))
  (labels ((recur (treap)
             (cond ((null treap)
                    (values nil nil))
                   ((funcall order (%treap-key treap) key)
                    (multiple-value-bind (left right) (recur (%treap-right treap))
                      (setf (%treap-right treap) left)
                      (update-count treap)
                      (values treap right)))
                   (t
                    (multiple-value-bind (left right) (recur (%treap-left treap))
                      (setf (%treap-left treap) right)
                      (update-count treap)
                      (values left treap))))))
    (recur treap)))

(declaim (inline treap-insert))
(defun treap-insert (key treap &key (order #'<))
  "Destructively inserts KEY into TREAP and returns the resultant treap."
  (declare ((or null treap) treap))
  (let ((node (%make-treap key (random most-positive-fixnum))))
    (labels ((recur (treap)
               (declare (treap node))
               (cond ((null treap) node)
                     ((> (%treap-priority node) (%treap-priority treap))
                      (setf (values (%treap-left node) (%treap-right node))
                            (treap-split (%treap-key node) treap :order order))
                      (update-count node)
                      node)
                     (t
                      (if (funcall order (%treap-key node) (%treap-key treap))
                          (setf (%treap-left treap)
                                (recur (%treap-left treap)))
                          (setf (%treap-right treap)
                                (recur (%treap-right treap))))
                      (update-count treap)
                      treap))))
      (recur treap))))

;; It takes O(nlog(n)).
(defun treap (order &rest keys)
  (loop with res = nil
        for key in keys
        do (setf res (treap-insert key res :order order))
        finally (return res)))

;; Reference: https://cp-algorithms.com/data_structures/treap.html
(declaim (inline make-treap))
(defun make-treap (sorted-vector)
  "Makes a treap from the given SORTED-VECTOR in O(n). Note that this function
doesn't check if the SORTED-VECTOR is actually sorted w.r.t. your intended
order. The consequence is undefined when a non-sorted vector is passed."
  (declare (vector sorted-vector))
  (labels ((heapify (top)
             (when top
               (let ((prioritized-node top))
                 (when (and (%treap-left top)
                            (> (%treap-priority (%treap-left top))
                               (%treap-priority prioritized-node)))
                   (setq prioritized-node (%treap-left top)))
                 (when (and (%treap-right top)
                            (> (%treap-priority (%treap-right top))
                               (%treap-priority prioritized-node)))
                   (setq prioritized-node (%treap-right top)))
                 (unless (eql prioritized-node top)
                   (rotatef (%treap-priority prioritized-node)
                            (%treap-priority top))
                   (heapify prioritized-node)))))
           (build (l r)
             (declare ((integer 0 #.most-positive-fixnum) l r))
             (if (= l r)
                 nil
                 (let* ((mid (ash (+ l r) -1))
                        (node (%make-treap (aref sorted-vector mid)
                                           (random most-positive-fixnum))))
                   (setf (%treap-left node) (build l mid))
                   (setf (%treap-right node) (build (+ mid 1) r))
                   (heapify node)
                   (update-count node)
                   node))))
    (build 0 (length sorted-vector))))

(defun treap-merge (left right)
  "Destructively concatenates two treaps. Assumes that all keys of LEFT are
smaller (or larger, depending on the order) than those of RIGHT.

Note that this `merge' is different from CL:MERGE and rather close to
CL:CONCATENATE. (TREAP-UNITE is the analogue of the former.)"
  (declare (optimize (speed 3))
           ((or null treap) left right))
  (cond ((null left) right)
        ((null right) left)
        ((> (%treap-priority left) (%treap-priority right))
         (setf (%treap-right left)
               (treap-merge (%treap-right left) right))
         (update-count left)
         left)
        (t
         (setf (%treap-left right)
               (treap-merge left (%treap-left right)))
         (update-count right)
         right)))

(declaim (inline treap-delete))
(defun treap-delete (key treap &key (order #'<))
  "Destructively deletes the KEY in TREAP and returns the resultant treap."
  (declare ((or null treap) treap))
  (labels ((recur (treap)
             (cond ((null treap) nil)
                   ((funcall order key (%treap-key treap))
                    (setf (%treap-left treap) (recur (%treap-left treap)))
                    (update-count treap)
                    treap)
                   ((funcall order (%treap-key treap) key)
                    (setf (%treap-right treap) (recur (%treap-right treap)))
                    (update-count treap)
                    treap)
                   (t
                    (treap-merge (%treap-left treap) (%treap-right treap))))))
    (declare (ftype (function * (values (or null treap) &optional)) recur))
    (recur treap)))

(defun treap-map (function treap)
  "Successively applies FUNCTION to TREAP[0], ..., TREAP[SIZE-1]. FUNCTION must
take one argument."
  (declare (function function))
  (when treap
    (treap-map function (%treap-left treap))
    (funcall function (%treap-key treap))
    (treap-map function (%treap-right treap))))

(defmethod print-object ((object treap) stream)
  (print-unreadable-object (object stream :type t)
    (let ((init t))
      (treap-map (lambda (key)
                   (if init
                       (setq init nil)
                       (write-char #\  stream))
                   (write key :stream stream))
                 object))))

(define-condition invalid-treap-index-error (type-error)
  ((treap :initarg :treap :reader invalid-treap-index-error-treap)
   (index :initarg :index :reader invalid-treap-index-error-index))
  (:report
   (lambda (condition stream)
     (format stream "Invalid index ~W for treap ~W."
             (invalid-treap-index-error-index condition)
             (invalid-treap-index-error-treap condition)))))

(defun treap-ref (treap index)
  "Index access"
  (declare ((or null treap) treap)
           ((integer 0 #.most-positive-fixnum) index))
  (when (>= index (treap-count treap))
    (error 'invalid-treap-index-error :treap treap :index index))
  (labels ((%ref (treap index)
             (declare (optimize (speed 3) (safety 0))
                      ((integer 0 #.most-positive-fixnum) index))
             (let ((left-count (treap-count (%treap-left treap))))
               (cond ((< index left-count)
                      (%ref (%treap-left treap) index))
                     ((> index left-count)
                      (%ref (%treap-right treap) (- index left-count 1)))
                     (t (%treap-key treap))))))
    (%ref treap index)))

(declaim (inline treap-unite))
(defun treap-unite (treap1 treap2 &key (order #'<))
  "Merges two trees with keeping the order."
  (labels
      ((recur (l r)
         (cond ((null l) r)
               ((null r) l)
               (t (when (< (%treap-priority l) (%treap-priority r))
                    (rotatef l r))
                  (multiple-value-bind (lchild rchild)
                      (treap-split (%treap-key l) r :order order)
                    (setf (%treap-left l) (recur (%treap-left l) lchild)
                          (%treap-right l) (recur (%treap-right l) rchild))
                    l)))))
    (recur treap1 treap2)))
