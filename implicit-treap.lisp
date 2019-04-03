(setf *print-circle* t)

;; Treap with implicit key for updating and querying interval.

(declaim (inline op))
(defun op (a b)
  (min a b))

(defconstant +updater-identity+ 0)

(declaim (inline updater-op))
(defun updater-op (a b)
  "Is the operator to compute and update LAZY value."
  (+ a b))

(declaim (inline modifier-op))
(defun modifier-op (a b size)
  "Is the operator to update ACCUMULATOR based on LAZY value."
  (declare (ignore size))
  (+ a b))

(defstruct (inode (:constructor %make-inode (value priority &key left right (count 1) (accumulator 0) (lazy 0) reversed))
                  (:copier nil)
                  (:conc-name %inode-))
  (value 0 :type fixnum)
  (accumulator 0 :type fixnum) ; e.g. MIN, MAX, SUM, ...
  (lazy +updater-identity+ :type fixnum)
  (reversed nil :type boolean)
  (priority 0 :type (integer 0 #.most-positive-fixnum))
  (count 1 :type (integer 0 #.most-positive-fixnum))
  (left nil :type (or null inode))
  (right nil :type (or null inode)))

(declaim (inline inode-count))
(defun inode-count (inode)
  (declare ((or null inode) inode))
  (if inode
      (%inode-count inode)
      0))

(declaim (inline update-count))
(defun update-count (inode)
  (declare (inode inode))
  (setf (%inode-count inode)
        (+ 1
           (inode-count (%inode-left inode))
           (inode-count (%inode-right inode)))))

(declaim (inline update-accumulator))
(defun update-accumulator (inode)
  (declare (inode inode))
  (setf (%inode-accumulator inode)
        (if (%inode-left inode)
            (if (%inode-right inode)
                (funcall #'op
                         (%inode-accumulator (%inode-left inode))
                         (funcall #'op
                                  (%inode-value inode)
                                  (%inode-accumulator (%inode-right inode))))
                (funcall #'op
                         (%inode-accumulator (%inode-left inode))
                         (%inode-value inode)))
            (if (%inode-right inode)
                (funcall #'op
                         (%inode-value inode)
                         (%inode-accumulator (%inode-right inode)))
                (%inode-value inode)))))

(declaim (inline force-self))
(defun force-self (inode)
  (declare (inode inode))
  (update-count inode)
  (update-accumulator inode))

(declaim (inline force-down))
(defun force-down (inode)
  (declare (inode inode))
  (when (%inode-reversed inode)
    (setf (%inode-reversed inode) nil)
    (rotatef (%inode-left inode) (%inode-right inode))
    (let ((left (%inode-left inode)))
      (when left
        (setf (%inode-reversed left) (not (%inode-reversed left)))))
    (let ((right (%inode-right inode)))
      (when right
        (setf (%inode-reversed right) (not (%inode-reversed right))))))
  (unless (eql +updater-identity+ (%inode-lazy inode))
    (when (%inode-left inode)
      (setf (%inode-lazy (%inode-left inode))
            (funcall #'updater-op
                     (%inode-lazy (%inode-left inode))
                     (%inode-lazy inode)))
      (setf (%inode-accumulator (%inode-left inode))
            (funcall #'modifier-op
                     (%inode-accumulator (%inode-left inode))
                     (%inode-lazy inode)
                     (%inode-count (%inode-left inode)))))
    (when (%inode-right inode)
      (setf (%inode-lazy (%inode-right inode))
            (funcall #'updater-op
                     (%inode-lazy (%inode-right inode))
                     (%inode-lazy inode)))
      (setf (%inode-accumulator (%inode-right inode))
            (funcall #'modifier-op
                     (%inode-accumulator (%inode-right inode))
                     (%inode-lazy inode)
                     (%inode-count (%inode-right inode)))))
    (setf (%inode-value inode)
          (funcall #'modifier-op
                   (%inode-value inode)
                   (%inode-lazy inode)
                   1))
    (setf (%inode-lazy inode) +updater-identity+)))

(defun inode-split (inode index)
  "Destructively splits the INODE into two nodes [0, INDEX) and [INDEX, N), where N
  is the number of elements of the INODE."
  (declare ((integer 0 #.most-positive-fixnum) index))
  (unless inode
    (return-from inode-split (values nil nil)))
  (force-down inode)
  (let ((implicit-key (1+ (inode-count (%inode-left inode)))))
    (if (< index implicit-key)
        (multiple-value-bind (left right)
            (inode-split (%inode-left inode) index)
          (setf (%inode-left inode) right)
          (force-self inode)
          (values left inode))
        (multiple-value-bind (left right)
            (inode-split (%inode-right inode) (- index implicit-key))
          (setf (%inode-right inode) left)
          (force-self inode)
          (values inode right)))))

(defun inode-merge (left right)
  "Destructively merges two INODEs."
  (declare ((or null inode) left right))
  (cond ((null left) (when right (force-down right) (force-self right)) right)
        ((null right) (when left (force-down left) (force-self left)) left)
        (t (force-down left)
           (force-down right)
           (if (> (%inode-priority left) (%inode-priority right))
               (progn
                 (setf (%inode-right left)
                       (inode-merge (%inode-right left) right))
                 (force-self left)
                 left)
               (progn
                 (setf (%inode-left right)
                       (inode-merge left (%inode-left right)))
                 (force-self right)
                 right)))))

;; (define-condition invalid-itreap-index-error (type-error)
;;   ((itreap :initarg :itreap :reader invalid-itreap-index-error-itreap)
;;    (index :initarg :index :reader invalid-itreap-index-error-index))
;;   (:report
;;    (lambda (condition stream)
;;      (format stream "Invalid index ~W for itreap ~S."
;;              (invalid-itreap-index-error-index condition)
;;              (invalid-itreap-index-error-itreap condition)))))

(declaim (inline inode-insert))
(defun inode-insert (inode index obj)
  "Destructively inserts OBJ into INODE at INDEX."
  (declare ((or null inode) inode)
           ((integer 0 #.most-positive-fixnum) index))
  (assert (<= index (inode-count inode)))
  (let ((obj-inode (%make-inode obj (random most-positive-fixnum))))
    (multiple-value-bind (left right)
        (inode-split inode index)
      (inode-merge (inode-merge left obj-inode) right))))

(defun inode-map (function inode)
  "Successively applies FUNCTION to INODE[0], ..., INODE[SIZE-1]."
  (declare (function function))
  (when inode
    (force-down inode)
    (inode-map function (%inode-left inode))
    (funcall function (%inode-value inode))
    (inode-map function (%inode-right inode))
    (force-self inode)))

(defmethod print-object ((object inode) stream)
  (print-unreadable-object (object stream :type t)
    (let ((size (inode-count object))
          (index 0))
      (declare ((integer 0 #.most-positive-fixnum) index))
      (inode-map (lambda (x)
                   (princ x stream)
                   (incf index)
                   (when (< index size)
                     (write-char #\  stream)))
                 object))))

(defmacro do-inode ((var inode &optional result) &body body)
  "Successively binds INODE[0], ..., INODE[SIZE-1] to VAR and executes BODY."
  `(block nil
     (inode-map (lambda (,var) ,@body) ,inode)
     ,result))

(defun inode (&rest args)
  ;; TODO: Currently takes O(nlog(n)) time though it can be reduced to O(n).
  (labels ((recurse (list position inode)
             (declare ((integer 0 #.most-positive-fixnum) position))
             (if (null list)
                 inode
                 (recurse (cdr list)
                          (1+ position)
                          (inode-insert inode position (car list))))))
    (recurse args 0 nil)))

(declaim (inline log2-ceil))
(defun log2-ceil (n) (integer-length (- n 1)))

(defun make-inode (min-count initial-element)
    "Returns a treap whose length is at least MIN-COUNT. Time complexity is
O(n). (OP INITIAL-ELEMENT INITIAL-ELEMENT) must be equal to INITIAL-ELEMENT."
  (declare ((integer 0 #.most-positive-fixnum) min-count))
  (when (zerop min-count)
    (return-from make-inode nil))
  (let* ((max-depth (log2-ceil (1+ min-count))) ; Needs depth = log(N+1) for N nodes.
         (count (- (ash 1 max-depth) 1)) ; 2^d-1 nodes for depth d
         (width (floor most-positive-fixnum count)) ; step size of priorities
         )
    (labels ((recurse (depth)
               (declare ((integer 0 #.(integer-length most-positive-fixnum)) depth))
               (if (= depth max-depth)
                   nil
                   ;; the beginning node at depth = d has the 2^(d+1)-1-th highest
                   ;; priority (1-based)
                   (let* ((priority-index (- (ash 2 depth) 1))
                          ;; highest priority base = width * (count-1)
                          ;; lowest priority base = 0
                          (priority-base (* (- count priority-index) width))
                          ;; 2^d nodes exist at depth = d
                          (priority-width (* width (ash 1 depth))))
                     (declare ((integer 0 #.most-positive-fixnum)
                               priority-index priority-base priority-width))
                     (%make-inode initial-element
                                  (+ priority-base (random priority-width))
                                  :count (- (ash 1 (- max-depth depth)) 1)
                                  :left (recurse (1+ depth))
                                  :right (recurse (1+ depth)))))))
      (recurse 0))))

(declaim (inline inode-delete))
(defun inode-delete (inode index)
  (declare ((integer 0 #.most-positive-fixnum) index))
  (assert (< index (inode-count inode)))
  (multiple-value-bind (inode1 inode2)
      (inode-split inode (1+ index))
    (multiple-value-bind (inode1 _)
        (inode-split inode1 index)
      (declare (ignore _))
      (inode-merge inode1 inode2))))

(declaim (inline inode-ref))
(defun inode-ref (inode index)
  (declare ((integer 0 #.most-positive-fixnum) index))
  (assert (< index (inode-count inode)))
  (labels ((%ref (inode index)
             (declare ((integer 0 #.most-positive-fixnum) index))
             (force-down inode)
             (prog1
                 (let ((left-count (inode-count (%inode-left inode))))
                   (cond ((< index left-count)
                          (%ref (%inode-left inode) index))
                         ((> index left-count)
                          (%ref (%inode-right inode) (- index left-count 1)))
                         (t (%inode-value inode))))
               (force-self inode))))
    (%ref inode index)))

(declaim (inline (setf inode-ref)))
(defun (setf inode-ref) (new-value inode index)
  (declare ((integer 0 #.most-positive-fixnum) index))
  (assert (< index (inode-count inode)))
  (labels ((%set (inode index)
             (declare ((integer 0 #.most-positive-fixnum) index))
             (force-down inode)
             (prog1
                 (let ((left-count (inode-count (%inode-left inode))))
                   (cond ((< index left-count)
                          (%set (%inode-left inode) index))
                         ((> index left-count)
                          (%set (%inode-right inode) (- index left-count 1)))
                         (t (setf (%inode-value inode) new-value))))
               (force-self inode))))
    (%set inode index)
    new-value))

(defun copy-inode (inode)
  "For development. Recursively copies the whole INODEs."
  (if (null inode)
      nil
      (%make-inode (%inode-value inode)
                   (%inode-priority inode)
                   :left (copy-inode (%inode-left inode))
                   :right (copy-inode (%inode-right inode))
                   :count (%inode-count inode)
                   :accumulator (%inode-accumulator inode)
                   :lazy (%inode-lazy inode)
                   :reversed (%inode-reversed inode))))

;; FIXME: might be problematic when two priorities collide.
(declaim (inline inode-query))
(defun inode-query (inode l r)
  (declare ((integer 0 #.most-positive-fixnum) l r))
  (assert (< l r)) ;; TODO: would better return the identity element if L = R
  (assert (<= r (inode-count inode)))
  (multiple-value-bind (inode-0-l inode-l-n)
      (inode-split inode l)
    (multiple-value-bind (inode-l-r inode-r-n)
        (inode-split inode-l-n (- r l))
      (prog1 (%inode-accumulator inode-l-r)
        (inode-merge inode-0-l (inode-merge inode-l-r inode-r-n))))))

(declaim (inline inode-reverse))
(defun inode-reverse (inode l r)
  (declare ((integer 0 #.most-positive-fixnum) l r))
  (assert (and (<= l r) (<= r (inode-count inode))))
  (multiple-value-bind (inode-0-l inode-l-n)
      (inode-split inode l)
    (multiple-value-bind (inode-l-r inode-r-n)
        (inode-split inode-l-n (- r l))
      (setf (%inode-reversed inode-l-r) (not (%inode-reversed inode-l-r)))
      (inode-merge inode-0-l (inode-merge inode-l-r inode-r-n)))))

(declaim (inline inode-update))
(defun inode-update (inode x l r)
  "Updates INODE[i] := (OP INODE[i] X) for all i in [l, r)"
  (declare ((integer 0 #.most-positive-fixnum) l r))
  (assert (and (<= l r) (<= r (inode-count inode))))
  (multiple-value-bind (inode-0-l inode-l-n)
      (inode-split inode l)
    (multiple-value-bind (inode-l-r inode-r-n)
        (inode-split inode-l-n (- r l))
      (when inode-l-r
        (setf (%inode-lazy inode-l-r)
              (funcall #'updater-op (%inode-lazy inode-l-r) x)))
      (inode-merge inode-0-l (inode-merge inode-l-r inode-r-n)))))

;; Test
;; (let ((itreap1 (make-itreap 50 15 :count 5))
;;       (itreap2 (make-itreap 100 11 :count 3)))
;;   (setf (itreap-left itreap1) (make-itreap 30 5 :count 3))
;;   (setf (itreap-left (itreap-left itreap1)) (make-itreap 20 2 :count 1))
;;   (setf (itreap-right (itreap-left itreap1)) (make-itreap 40 4 :count 1))
;;   (setf (itreap-right itreap1) (make-itreap 70 10 :count 1))
;;   (setf (itreap-right itreap2) (make-itreap 200 3 :count 1))
;;   (setf (itreap-left itreap2) (make-itreap 99 5 :count 1))
;;   ;; split and merge
;;   (let ((itreap (itreap-merge (copy-itreap itreap1) (copy-itreap itreap2))))
;;     (assert (= 8 (itreap-count itreap)))
;;     (multiple-value-bind (left right) (itreap-split 80 (copy-itreap itreap))
;;       (assert (= 5 (itreap-count left)))
;;       (assert (= 3 (itreap-count right)))
;;       (assert (equalp itreap (itreap-merge left right)))))
;;   ;; find
;;   (assert (= 40 (itreap-find 40 itreap1)))
;;   (assert (null (itreap-find 41 itreap1)))
;;   ;; insert and delete
;;   (let ((inserted-itreap1 (itreap-insert 41 (copy-itreap itreap1))))
;;     (assert (= 41 (itreap-find 41 inserted-itreap1)))
;;     (let ((deleted-itreap1 (itreap-delete 41 inserted-itreap1)))
;;       (assert (null (itreap-find 41 deleted-itreap1)))
;;       (assert (equalp itreap1 deleted-itreap1))
;;       (assert (equalp itreap1 (itreap-delete 41 deleted-itreap1)))))
;;   (let ((itreap (itreap-merge itreap1 itreap2)))
;;     (assert (= 20 (itreap-ref itreap 0)))
;;     (assert (= 30 (itreap-ref itreap 1)))
;;     (assert (= 40 (itreap-ref itreap 2)))
;;     (assert (= 50 (itreap-ref itreap 3)))
;;     (assert (= 70 (itreap-ref itreap 4)))
;;     (assert (= 99 (itreap-ref itreap 5)))
;;     (assert (= 100 (itreap-ref itreap 6)))
;;     (assert (= 200 (itreap-ref itreap 7)))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload :fiveam)
  (use-package :fiveam))

(defun consistent-priorities-p (inode)
  (labels ((priority* (inode)
             (if (null inode) 0 (%inode-priority inode))))
    (declare (inline priority*))
    (if (null inode)
        t
        (and (> (%inode-priority inode) (priority* (%inode-left inode)))
             (> (%inode-priority inode) (priority* (%inode-right inode)))
             (consistent-priorities-p (%inode-left inode))
             (consistent-priorities-p (%inode-right inode))))))

(test :itreap
  (is (loop for i below 100 always (consistent-priorities-p (make-inode i 0)))))

(dotimes (i 10)
  (let ((itreap1 (inode 1 2 3 2 1 2 7)))
    (let (res)
      (do-inode (n itreap1) (push n res))
      (assert (equal (reverse res) '(1 2 3 2 1 2 7))))
    (assert (= 7 (inode-ref itreap1 6)))
    (assert (= 1 (inode-query itreap1 0 7)))
    (assert (= 2 (inode-query itreap1 5 7)))
    (assert (= 2 (inode-query itreap1 1 3)))
    (setf (inode-ref itreap1 1) 0)
    (assert (= 0 (inode-ref itreap1 1)))
    (assert (= 0 (inode-query itreap1 0 3)))
    (assert (= 0 (inode-query itreap1 0 7)))
    (assert (= 1 (inode-query itreap1 2 7)))))
