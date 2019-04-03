(setf *print-circle* t)

;; Treap with implicit key for updating point and querying interval or point.
;; This implementation includes the reverse operation of a interval though it
;; will rarely be used.

;; TODO: I have not introduced any abstractions yet though this structure can be
;; applied to any monoids.

(defstruct (inode (:constructor %make-inode (value priority &key left right (count 1) (accumulator 0) reversed))
                  (:copier nil)
                  (:conc-name %inode-))
  (value 0 :type fixnum)
  (accumulator 0 :type fixnum) ; e.g. MIN, MAX, SUM, ...
  (reversed nil :type boolean)
  (priority 0 :type (integer 0 #.most-positive-fixnum))
  (count 1 :type (integer 0 #.most-positive-fixnum))
  (left nil :type (or null inode))
  (right nil :type (or null inode)))

(defmacro op (&rest args)
  `(max ,@args))

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
                (op (%inode-accumulator (%inode-left inode))
                    (op (%inode-value inode)
                        (%inode-accumulator (%inode-right inode))))
                (op (%inode-accumulator (%inode-left inode))
                    (%inode-value inode)))
            (if (%inode-right inode)
                (op (%inode-value inode)
                    (%inode-accumulator (%inode-right inode)))
                (%inode-value inode)))))

(declaim (inline force-self))
(defun force-self (inode)
  "Propagates informations from children to INODE."
  (declare (inode inode))
  (update-count inode)
  (update-accumulator inode))

(declaim (inline force-down))
(defun force-down (inode)
  "Propagates the (lazy) reverse operation from INODE to children."
  (declare (inode inode))
  (when (%inode-reversed inode)
    (setf (%inode-reversed inode) nil)
    (rotatef (%inode-left inode) (%inode-right inode))
    (let ((left (%inode-left inode)))
      (when left
        (setf (%inode-reversed left) (not (%inode-reversed left)))))
    (let ((right (%inode-right inode)))
      (when right
        (setf (%inode-reversed right) (not (%inode-reversed right)))))))

(defun inode-split (inode index)
  "Destructively splits the INODE into two nodes [0, INDEX) and [INDEX, N), where N
  is the number of the elements of INODE."
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
        (t
         (force-down left)
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
  "iteratively calls FUNCTION for INODE[0] ... INODE[END-1]. O(n)."
  (declare (function function))
  (when inode
    (force-down inode)
    (inode-map function (%inode-left inode))
    (funcall function (%inode-value inode))
    (inode-map function (%inode-right inode))
    (force-self inode)))

(defmacro do-inode ((var inode &optional result) &body body)
  "DO-macro for INODE-MAP."
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
  "Returns a treap whose length is at least MIN-COUNT. Time complexity is O(n).

(OP INITIAL-ELEMENT INITIAL-ELEMENT) must be equal to INITIAL-ELEMENT."
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
                          (priority-width (* width (ash 1 depth)))
                          (left (recurse (1+ depth))))
                     (declare ((integer 0 #.most-positive-fixnum)
                               priority-index priority-base priority-width))
                     (%make-inode initial-element
                                  (+ priority-base (random priority-width))
                                  :count (- (ash 1 (- max-depth depth)) 1)
                                  :accumulator initial-element
                                  :left left
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
             (let ((left-count (inode-count (%inode-left inode))))
               (cond ((< index left-count)
                      (%set (%inode-left inode) index))
                     ((> index left-count)
                      (%set (%inode-right inode) (- index left-count 1)))
                     (t (setf (%inode-value inode) new-value))))
             (force-self inode)))
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
                   :reversed (%inode-reversed inode))))

;; FIXME: might be problematic when two priorities collide at the top of the
;; treap. Ultimately we will need another structure that holds the pointer to
;; the top.
(declaim (inline inode-query))
(defun inode-query (inode l r)
  (declare ((integer 0 #.most-positive-fixnum) l r))
  (assert (< l r)) ; TODO: would better return the identity element if L = R
  (multiple-value-bind (inode-0-l inode-l-n)
      (inode-split inode l)
    (multiple-value-bind (inode-l-r inode-r-n)
        (inode-split inode-l-n (- r l))
      (prog1 (%inode-accumulator inode-l-r)
        (inode-merge inode-0-l (inode-merge inode-l-r inode-r-n))))))

(declaim (inline inode-reverse))
(defun inode-reverse (inode l r)
  (declare ((integer 0 #.most-positive-fixnum) l r))
  (assert (<= l r))
  (multiple-value-bind (inode-0-l inode-l-n)
      (inode-split inode l)
    (multiple-value-bind (inode-l-r inode-r-n)
        (inode-split inode-l-n (- r l))
      (setf (%inode-reversed inode-l-r) (not (%inode-reversed inode-l-r)))
      (inode-merge inode-0-l (inode-merge inode-l-r inode-r-n)))))

;;;
;;;  Test
;;;

(dotimes (i 10)
  (let ((itreap1 (inode 1 2 3 2 1 2 7)))
    (let (res)
      (do-inode (n itreap1) (push n res))
      (assert (equal (reverse res) '(1 2 3 2 1 2 7))))
    (assert (= 7 (inode-ref itreap1 6)))
    (assert (= 7 (inode-query itreap1 0 7)))
    (assert (= 7 (inode-query itreap1 5 7)))
    (assert (= 3 (inode-query itreap1 0 3)))
    (setf (inode-ref itreap1 1) 20)
    (assert (= 20 (inode-ref itreap1 1)))
    (assert (= 20 (inode-query itreap1 0 3)))
    (assert (= 20 (inode-query itreap1 0 7)))))

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

(assert (loop for i below 100 always (consistent-priorities-p (make-inode i 0))))
