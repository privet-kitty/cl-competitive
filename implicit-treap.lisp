;;;
;;; Implicit treap
;;; (treap with implicit key)
;;;

;; Note:
;; - You cannot rely on the side effect when you call any destructive operations
;; on a treap. Always use the returned value.
;; - An empty treap is NIL.

(defconstant +op-identity+ most-positive-fixnum)

(declaim (inline op))
(defun op (a b)
  (min a b))

(defconstant +updater-identity+ 0)

(declaim (inline updater-op))
(defun updater-op (a b)
  "Is the operator to compute and update LAZY value. A is the current LAZY value
and B is operand."
  (+ a b))

(declaim (inline modifier-op))
(defun modifier-op (acc x size)
  "Is the operator to update ACCUMULATOR (and VALUE) based on LAZY value. ACC is
the current ACCUMULATOR value and X is the LAZY value. SIZE is the length of the
specified interval."
  (declare (ignorable size))
  (+ acc x))

(defstruct (itreap (:constructor %make-itreap (value priority &key left right (count 1) (accumulator value) (lazy +updater-identity+) reversed))
                  (:copier nil)
                  (:conc-name %itreap-))
  (value +op-identity+ :type fixnum)
  (accumulator +op-identity+ :type fixnum)
  (lazy +updater-identity+ :type fixnum)
  (reversed nil :type boolean)
  (priority 0 :type (integer 0 #.most-positive-fixnum))
  (count 1 :type (integer 0 #.most-positive-fixnum)) ; size of (sub)treap
  (left nil :type (or null itreap))
  (right nil :type (or null itreap)))

(declaim (inline itreap-count))
(defun itreap-count (itreap)
  (declare ((or null itreap) itreap))
  (if itreap
      (%itreap-count itreap)
      0))

(declaim (inline itreap-accumulator))
(defun itreap-accumulator (itreap)
  (declare ((or null itreap) itreap))
  (if itreap
      (%itreap-accumulator itreap)
      +op-identity+))

(declaim (inline update-count))
(defun update-count (itreap)
  (declare (itreap itreap))
  (setf (%itreap-count itreap)
        (+ 1
           (itreap-count (%itreap-left itreap))
           (itreap-count (%itreap-right itreap)))))

(declaim (inline update-accumulator))
(defun update-accumulator (itreap)
  (declare (itreap itreap))
  (setf (%itreap-accumulator itreap)
        (if (%itreap-left itreap)
            (if (%itreap-right itreap)
                (let ((mid (op (%itreap-accumulator (%itreap-left itreap))
                               (%itreap-value itreap))))
                  (declare (dynamic-extent mid))
                  (op mid (%itreap-accumulator (%itreap-right itreap))))
                (op (%itreap-accumulator (%itreap-left itreap))
                    (%itreap-value itreap)))
            (if (%itreap-right itreap)
                (op (%itreap-value itreap)
                    (%itreap-accumulator (%itreap-right itreap)))
                (%itreap-value itreap)))))

(declaim (inline force-self))
(defun force-self (itreap)
  (declare (itreap itreap))
  (update-count itreap)
  (update-accumulator itreap))

(declaim (inline force-down))
(defun force-down (itreap)
  (declare (itreap itreap))
  (when (%itreap-reversed itreap)
    (setf (%itreap-reversed itreap) nil)
    (rotatef (%itreap-left itreap) (%itreap-right itreap))
    (let ((left (%itreap-left itreap)))
      (when left
        (setf (%itreap-reversed left) (not (%itreap-reversed left)))))
    (let ((right (%itreap-right itreap)))
      (when right
        (setf (%itreap-reversed right) (not (%itreap-reversed right))))))
  (unless (eql +updater-identity+ (%itreap-lazy itreap))
    (when (%itreap-left itreap)
      (setf (%itreap-lazy (%itreap-left itreap))
            (updater-op (%itreap-lazy (%itreap-left itreap))
                        (%itreap-lazy itreap)))
      (setf (%itreap-accumulator (%itreap-left itreap))
            (modifier-op (%itreap-accumulator (%itreap-left itreap))
                         (%itreap-lazy itreap)
                         (%itreap-count (%itreap-left itreap)))))
    (when (%itreap-right itreap)
      (setf (%itreap-lazy (%itreap-right itreap))
            (updater-op (%itreap-lazy (%itreap-right itreap))
                        (%itreap-lazy itreap)))
      (setf (%itreap-accumulator (%itreap-right itreap))
            (modifier-op (%itreap-accumulator (%itreap-right itreap))
                         (%itreap-lazy itreap)
                         (%itreap-count (%itreap-right itreap)))))
    (setf (%itreap-value itreap)
          (modifier-op (%itreap-value itreap)
                       (%itreap-lazy itreap)
                       1))
    (setf (%itreap-lazy itreap) +updater-identity+)))

(defun itreap-split (itreap index)
  "Destructively splits the ITREAP into two nodes [0, INDEX) and [INDEX, N), where N
  is the number of elements of the ITREAP."
  (declare (optimize (speed 3))
           ((integer 0 #.most-positive-fixnum) index))
  (unless (<= index (itreap-count itreap))
    (error 'invalid-itreap-index-error :index index :itreap itreap))
  (labels ((recur (itreap ikey)
             (unless itreap
               (return-from itreap-split (values nil nil)))
             (force-down itreap)
             (let ((left-count (itreap-count (%itreap-left itreap))))
               (if (<= ikey left-count)
                   (multiple-value-bind (left right)
                       (itreap-split (%itreap-left itreap) ikey)
                     (setf (%itreap-left itreap) right)
                     (force-self itreap)
                     (values left itreap))
                   (multiple-value-bind (left right)
                       (itreap-split (%itreap-right itreap) (- ikey left-count 1))
                     (setf (%itreap-right itreap) left)
                     (force-self itreap)
                     (values itreap right))))))
    (recur itreap index)))

(defun itreap-merge (left right)
  "Destructively merges two ITREAPs."
  (declare (optimize (speed 3))
           ((or null itreap) left right))
  (cond ((null left) (when right (force-down right) (force-self right)) right)
        ((null right) (when left (force-down left) (force-self left)) left)
        (t (force-down left)
           (force-down right)
           (if (> (%itreap-priority left) (%itreap-priority right))
               (progn
                 (setf (%itreap-right left)
                       (itreap-merge (%itreap-right left) right))
                 (force-self left)
                 left)
               (progn
                 (setf (%itreap-left right)
                       (itreap-merge left (%itreap-left right)))
                 (force-self right)
                 right)))))

(define-condition invalid-itreap-index-error (type-error)
  ((itreap :initarg :itreap :reader invalid-itreap-index-error-itreap)
   (index :initarg :index :reader invalid-itreap-index-error-index))
  (:report
   (lambda (condition stream)
     (let ((index (invalid-itreap-index-error-index condition)))
       (if (consp index)
           (format stream "Invalid range [~W, ~W) for itreap ~W."
                   (car index)
                   (cdr index)
                   (invalid-itreap-index-error-itreap condition))
           (format stream "Invalid index ~W for itreap ~W."
                   index
                   (invalid-itreap-index-error-itreap condition)))))))

(defun itreap-insert (itreap index obj)
  "Destructively inserts OBJ into ITREAP and returns the resultant treap."
  (declare (optimize (speed 3))
           ((or null itreap) itreap)
           ((integer 0 #.most-positive-fixnum) index))
  (unless (<= index (itreap-count itreap))
    (error 'invalid-itreap-index-error :itreap itreap :index index))
  (let ((node (%make-itreap obj (random most-positive-fixnum))))
    (labels ((recur (itreap ikey)
               (declare ((integer 0 #.most-positive-fixnum) ikey))
               (unless itreap (return-from recur node))
               (force-down itreap)
               (if (> (%itreap-priority node) (%itreap-priority itreap))
                   (progn
                     (setf (values (%itreap-left node) (%itreap-right node))
                           (itreap-split itreap ikey))
                     (force-self node)
                     node)
                   (let ((left-count (itreap-count (%itreap-left itreap))))
                     (if (<= ikey left-count)
                         (setf (%itreap-left itreap)
                               (recur (%itreap-left itreap) ikey))
                         (setf (%itreap-right itreap)
                               (recur (%itreap-right itreap) (- ikey left-count 1))))
                     (force-self itreap)
                     itreap))))
      (recur itreap index))))

(declaim (inline itreap-map))
(defun itreap-map (function itreap)
  "Successively applies FUNCTION to ITREAP[0], ..., ITREAP[SIZE-1]."
  (declare (function function))
  (labels ((recur (node)
             (when node
               (force-down node)
               (recur (%itreap-left node))
               (funcall function (%itreap-value node))
               (recur (%itreap-right node))
               (force-self node))))
    (recur itreap)))

(defmethod print-object ((object itreap) stream)
  (print-unreadable-object (object stream :type t)
    (let ((init t))
      (itreap-map (lambda (x)
                    (if init
                        (setq init nil)
                        (write-char #\  stream))
                   (write x :stream stream))
                 object))))

(defmacro do-itreap ((var itreap &optional result) &body body)
  "Successively binds ITREAP[0], ..., ITREAP[SIZE-1] to VAR and executes BODY
each time."
  `(block nil
     (itreap-map (lambda (,var) ,@body) ,itreap)
     ,result))

(defun itreap (&rest args)
  ;; NOTE: It takes O(nlog(n)). Use MAKE-ITREAP for efficiency.
  (labels ((recur (list position itreap)
             (declare ((integer 0 #.most-positive-fixnum) position))
             (if (null list)
                 itreap
                 (recur (cdr list)
                        (1+ position)
                        (itreap-insert itreap position (car list))))))
    (recur args 0 nil)))

(defun %heapify (top)
  "Properly swaps the priorities of the node and its two children."
  (declare (optimize (speed 3) (safety 0)))
  (when top
    (let ((high-priority-node top))
      (when (and (%itreap-left top)
                 (> (%itreap-priority (%itreap-left top))
                    (%itreap-priority high-priority-node)))
        (setq high-priority-node (%itreap-left top)))
      (when (and (%itreap-right top)
                 (> (%itreap-priority (%itreap-right top))
                    (%itreap-priority high-priority-node)))
        (setq high-priority-node (%itreap-right top)))
      (unless (eql high-priority-node top)
        (rotatef (%itreap-priority high-priority-node)
                 (%itreap-priority top))
        (%heapify high-priority-node)))))

(declaim (inline make-itreap))
(defun make-itreap (size &key initial-contents)
  "Makes a treap of SIZE in O(SIZE) time. The values are filled with the
identity element unless INITIAL-CONTENTS are supplied."
  (declare ((or null vector) initial-contents))
  (labels ((build (l r)
             (declare ((integer 0 #.most-positive-fixnum) l r))
             (if (= l r)
                 nil
                 (let* ((mid (ash (+ l r) -1))
                        (node (%make-itreap (if initial-contents
                                                (aref initial-contents mid)
                                                +op-identity+)
                                           (random most-positive-fixnum))))
                   (setf (%itreap-left node) (build l mid))
                   (setf (%itreap-right node) (build (+ mid 1) r))
                   (%heapify node)
                   (force-self node)
                   node))))
    (build 0 size)))

(defun itreap-delete (itreap index)
  "Destructively deletes the object at INDEX in ITREAP."
  (declare (optimize (speed 3))
           ((integer 0 #.most-positive-fixnum) index))
  (unless (< index (itreap-count itreap))
    (error 'invalid-itreap-index-error :itreap itreap :index index))
  (labels ((recur (itreap ikey)
             (declare ((integer 0 #.most-positive-fixnum) ikey))
             (force-down itreap)
             (let ((left-count (itreap-count (%itreap-left itreap))))
               (cond ((< ikey left-count)
                      (setf (%itreap-left itreap)
                            (recur (%itreap-left itreap) ikey))
                      (force-self itreap)
                      itreap)
                     ((> ikey left-count)
                      (setf (%itreap-right itreap)
                            (recur (%itreap-right itreap) (- ikey left-count 1)))
                      (force-self itreap)
                      itreap)
                     (t
                      (itreap-merge (%itreap-left itreap) (%itreap-right itreap)))))))
    (recur itreap index)))

(declaim (inline itreap-ref))
(defun itreap-ref (itreap index)
  (declare ((integer 0 #.most-positive-fixnum) index))
  (unless (< index (itreap-count itreap))
    (error 'invalid-itreap-index-error :itreap itreap :index index))
  (labels ((%ref (itreap index)
             (declare ((integer 0 #.most-positive-fixnum) index))
             (force-down itreap)
             (prog1
                 (let ((left-count (itreap-count (%itreap-left itreap))))
                   (cond ((< index left-count)
                          (%ref (%itreap-left itreap) index))
                         ((> index left-count)
                          (%ref (%itreap-right itreap) (- index left-count 1)))
                         (t (%itreap-value itreap))))
               (force-self itreap))))
    (%ref itreap index)))

(declaim (inline (setf itreap-ref)))
(defun (setf itreap-ref) (new-value itreap index)
  (declare ((integer 0 #.most-positive-fixnum) index))
  (unless (< index (itreap-count itreap))
    (error 'invalid-itreap-index-error :itreap itreap :index index))
  (labels ((%set (itreap index)
             (declare ((integer 0 #.most-positive-fixnum) index))
             (force-down itreap)
             (prog1
                 (let ((left-count (itreap-count (%itreap-left itreap))))
                   (cond ((< index left-count)
                          (%set (%itreap-left itreap) index))
                         ((> index left-count)
                          (%set (%itreap-right itreap) (- index left-count 1)))
                         (t (setf (%itreap-value itreap) new-value))))
               (force-self itreap))))
    (%set itreap index)
    new-value))

;; FIXME: might be problematic when two priorities collide.
(declaim (inline itreap-query))
(defun itreap-query (itreap l r)
  (declare ((integer 0 #.most-positive-fixnum) l r))
  (unless (<= l r (itreap-count itreap))
    (error 'invalid-itreap-index-error :itreap itreap :index (cons l r)))
  (if (= l r)
      +op-identity+
      (multiple-value-bind (itreap-0-l itreap-l-n)
          (itreap-split itreap l)
        (multiple-value-bind (itreap-l-r itreap-r-n)
            (itreap-split itreap-l-n (- r l))
          (prog1 (%itreap-accumulator itreap-l-r)
            (itreap-merge itreap-0-l (itreap-merge itreap-l-r itreap-r-n)))))))

;; Below is faster but hard to maintain
;; (defun itreap-query (itreap &optional (start 0) end)
;;   "Queries the sum of the half-open interval specified by the index: [START,
;; END). If END is not given, it is assumed to be the length of ITREAP]."
;;   (declare ((integer 0 #.most-positive-fixnum) start)
;;            ((or null (integer 0 #.most-positive-fixnum)) end))
;;   (if (zerop start)
;;       (if (null end)
;;           (itreap-accumulator itreap)
;;           (if (> end (itreap-count itreap))
;;               (error 'invalid-itreap-index-error :itreap itreap :index (cons 0 end))
;;               (multiple-value-bind (itreap-0-r itreap-r-n)
;;                   (itreap-split itreap end)
;;                 (prog1 (itreap-accumulator itreap-0-r)
;;                   (itreap-merge itreap-0-r itreap-r-n)))))
;;       (if (null end)
;;           (if (> start (itreap-count itreap))
;;               (error 'invalid-itreap-index-error :itreap itreap :index (cons start (itreap-count itreap)))
;;               (multiple-value-bind (itreap-0-l itreap-l-n)
;;                   (itreap-split itreap start)
;;                 (prog1 (itreap-accumulator itreap-l-n)
;;                   (itreap-merge itreap-0-l itreap-l-n))))
;;           (progn
;;             (unless (<= start end (itreap-count itreap))
;;               (error 'invalid-itreap-index-error :itreap itreap :index (cons start end)))
;;             (multiple-value-bind (itreap-0-l itreap-l-n)
;;                 (itreap-split itreap start)
;;               (multiple-value-bind (itreap-l-r itreap-r-n)
;;                   (itreap-split itreap-l-n (- end start))
;;                 (prog1 (itreap-accumulator itreap-l-r)
;;                   (itreap-merge itreap-0-l (itreap-merge itreap-l-r itreap-r-n)))))))))

(declaim (inline itreap-reverse))
(defun itreap-reverse (itreap l r)
  (declare ((integer 0 #.most-positive-fixnum) l r))
  (unless (<= l r (itreap-count itreap))
    (error 'invalid-itreap-index-error :itreap itreap :index (cons l r)))
  (multiple-value-bind (itreap-0-l itreap-l-n)
      (itreap-split itreap l)
    (multiple-value-bind (itreap-l-r itreap-r-n)
        (itreap-split itreap-l-n (- r l))
      (setf (%itreap-reversed itreap-l-r) (not (%itreap-reversed itreap-l-r)))
      (itreap-merge itreap-0-l (itreap-merge itreap-l-r itreap-r-n)))))

(declaim (inline itreap-update))
(defun itreap-update (itreap operand l r)
  "Updates ITREAP[i] := (OP ITREAP[i] OPERAND) for all i in [l, r)"
  (declare ((integer 0 #.most-positive-fixnum) l r))
  (unless (<= l r (itreap-count itreap))
    (error 'invalid-itreap-index-error :itreap itreap :index (cons l r)))
  (multiple-value-bind (itreap-0-l itreap-l-n)
      (itreap-split itreap l)
    (multiple-value-bind (itreap-l-r itreap-r-n)
        (itreap-split itreap-l-n (- r l))
      (when itreap-l-r
        (setf (%itreap-lazy itreap-l-r)
              (updater-op (%itreap-lazy itreap-l-r) operand)))
      (itreap-merge itreap-0-l (itreap-merge itreap-l-r itreap-r-n)))))

(declaim (inline itreap-bisect-left))
(defun itreap-bisect-left (itreap threshold order)
  "Takes a **sorted** treap and returns the smallest index that satisfies
ITREAP[index] >= THRESHOLD, where >= is the complement of ORDER. Returns the
size of ITREAP if ITREAP[length-1] < THRESHOLD. The time complexity is
O(log(n))."
  (declare (function order))
  (labels ((recur (count itreap)
             (declare ((integer 0 #.most-positive-fixnum) count))
             (cond ((null itreap) nil)
                   ((funcall order (%itreap-value itreap) threshold)
                    (recur count (%itreap-right itreap)))
                   (t
                    (let ((left-count (- count (itreap-count (%itreap-right itreap)) 1)))
                      (or (recur left-count (%itreap-left itreap))
                          left-count))))))
    (or (recur (itreap-count itreap) itreap)
        (itreap-count itreap))))

(declaim (inline itreap-insort))
(defun itreap-insort (itreap obj order)
  "Does insertion to the sorted treap."
  (let ((pos (itreap-bisect-left itreap obj order)))
    (itreap-insert itreap pos obj)))
