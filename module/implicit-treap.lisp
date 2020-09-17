;;;
;;; Implicit treap
;;; (treap with implicit key)
;;;

(defpackage :cp/implicit-treap
  (:use :cl)
  (:export #:itreap #:itreap-p #:itreap-count #:itreap-accumulator
           #:make-itreap #:invalid-itreap-index-error #:itreap-ref
           #:itreap-split #:itreap-merge #:itreap-insert #:itreap-delete
           #:itreap-push #:itreap-pop #:itreap-map #:do-itreap
           #:itreap-fold #:itreap-fold-bisect #:itreap-fold-bisect-from-end
           #:itreap-update #:itreap-reverse
           #:itreap-bisect-left #:itreap-bisect-right #:itreap-insort))
(in-package :cp/implicit-treap)

;; Note:
;; - An empty treap is NIL.

(declaim (inline op))
(defun op (a b)
  "Is a binary operator comprising a monoid."
  (min a b))

(defconstant +op-identity+ most-positive-fixnum
  "identity element w.r.t. OP")

(declaim (inline updater-op))
(defun updater-op (a b)
  "Is the operator to compute and update LAZY value. A is the current LAZY value
and B is operand."
  (+ a b))

(defconstant +updater-identity+ 0
  "identity element w.r.t. UPDATER-OP")

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
  "Returns the number of the elements."
  (declare ((or null itreap) itreap))
  (if itreap
      (%itreap-count itreap)
      0))

(declaim (inline itreap-accumulator))
(defun itreap-accumulator (itreap)
  "Returns the sum (w.r.t. OP) of the whole ITREAP:
ITREAP[0]+ITREAP[1]+...+ITREAP[SIZE-1]."
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

(declaim (inline force-up))
(defun force-up (itreap)
  "Propagates up the information from children."
  (declare (itreap itreap))
  (update-count itreap)
  (update-accumulator itreap))

(declaim (inline force-down))
(defun force-down (itreap)
  "Propagates down the information to children."
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
  "Makes a treap of SIZE in O(SIZE) time. Its values are filled with the
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
                   (force-up node)
                   node))))
    (build 0 size)))

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

(defun itreap-split (itreap index)
  "Destructively splits the ITREAP into two nodes [0, INDEX) and [INDEX, N),
where N is the number of the elements in ITREAP."
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
                     (force-up itreap)
                     (values left itreap))
                   (multiple-value-bind (left right)
                       (itreap-split (%itreap-right itreap) (- ikey left-count 1))
                     (setf (%itreap-right itreap) left)
                     (force-up itreap)
                     (values itreap right))))))
    (recur itreap index)))

(defun itreap-merge (left right)
  "Destructively concatenates two ITREAPs."
  (declare (optimize (speed 3))
           ((or null itreap) left right))
  (cond ((null left) (when right (force-down right) (force-up right)) right)
        ((null right) (when left (force-down left) (force-up left)) left)
        (t (force-down left)
           (force-down right)
           (if (> (%itreap-priority left) (%itreap-priority right))
               (progn
                 (setf (%itreap-right left)
                       (itreap-merge (%itreap-right left) right))
                 (force-up left)
                 left)
               (progn
                 (setf (%itreap-left right)
                       (itreap-merge left (%itreap-left right)))
                 (force-up right)
                 right)))))

(defun itreap-insert (itreap index obj)
  "Destructively inserts OBJ into ITREAP and returns the resultant treap.

You cannot rely on the side effect. Use the returned value."
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
                     (force-up node)
                     node)
                   (let ((left-count (itreap-count (%itreap-left itreap))))
                     (if (<= ikey left-count)
                         (setf (%itreap-left itreap)
                               (recur (%itreap-left itreap) ikey))
                         (setf (%itreap-right itreap)
                               (recur (%itreap-right itreap) (- ikey left-count 1))))
                     (force-up itreap)
                     itreap))))
      (recur itreap index))))

(defun itreap-delete (itreap index)
  "Destructively deletes the object at INDEX in ITREAP.

You cannot rely on the side effect. Use the returned value."
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
                      (force-up itreap)
                      itreap)
                     ((> ikey left-count)
                      (setf (%itreap-right itreap)
                            (recur (%itreap-right itreap) (- ikey left-count 1)))
                      (force-up itreap)
                      itreap)
                     (t
                      (itreap-merge (%itreap-left itreap) (%itreap-right itreap)))))))
    (recur itreap index)))

(defmacro itreap-push (obj itreap pos)
  "Pushes OBJ to ITREAP at POS."
  `(setf ,itreap (itreap-insert ,itreap ,pos ,obj)))

(defmacro itreap-pop (itreap pos)
  "Returns the object at POS and deletes it."
  (let ((p (gensym)))
    `(let ((,p ,pos))
       (prog1 (itreap-ref ,itreap ,p)
         (setf ,itreap (itreap-delete ,itreap ,p))))))

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
               (force-up node))))
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
  ;; NOTE: This function takes O(nlog(n)) time. Use MAKE-ITREAP for efficiency.
  (labels ((recur (list position itreap)
             (declare ((integer 0 #.most-positive-fixnum) position))
             (if (null list)
                 itreap
                 (recur (cdr list)
                        (1+ position)
                        (itreap-insert itreap position (car list))))))
    (recur args 0 nil)))

(declaim (inline itreap-ref))
(defun itreap-ref (itreap index)
  "Returns the element ITREAP[INDEX]."
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
               (force-up itreap))))
    (%ref itreap index)))

(declaim (inline (setf itreap-ref)))
(defun (setf itreap-ref) (new-value itreap index)
  "Sets ITREAP[INDEX] to the given value."
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
               (force-up itreap))))
    (%set itreap index)
    new-value))

(declaim (inline itreap-fold))
(defun itreap-fold (itreap l r)
  "Queries the `sum' (w.r.t. OP) of the range ITREAP[L, R)."
  (declare ((integer 0 #.most-positive-fixnum) l r))
  (unless (<= l r (itreap-count itreap))
    (error 'invalid-itreap-index-error :itreap itreap :index (cons l r)))
  (labels
      ((recur (itreap l r)
         (declare ((integer 0 #.most-positive-fixnum) l r))
         (unless itreap
           (return-from recur +op-identity+))
         (force-down itreap)
         (prog1
             (if (and (zerop l) (= r (%itreap-count itreap)))
                 (itreap-accumulator itreap)
                 (let ((left-count (itreap-count (%itreap-left itreap))))
                   (if (<= l left-count)
                       (if (< left-count r)
                           ;; LEFT-COUNT is in [L, R)
                           (op (op (recur (%itreap-left itreap) l (min r left-count))
                                   (%itreap-value itreap))
                               (recur (%itreap-right itreap) 0 (- r left-count 1)))
                           ;; LEFT-COUNT is in [R, END)
                           (recur (%itreap-left itreap) l (min r left-count)))
                       ;; LEFT-COUNT is in [0, L)
                       (recur (%itreap-right itreap) (- l left-count 1) (- r left-count 1)))))
           (force-up itreap))))
    (recur itreap l r)))

;; FIXME: might be problematic when two priorities collide and START is not
;; zero. (It will be negligible from the viewpoint of probability, however.)
(declaim (inline itreap-fold-bisect))
(defun itreap-fold-bisect (itreap test &optional (start 0))
  "Returns the largest index that satisfies (FUNCALL TEST (OP ITREAP[START]
ITREAP[START+1] ... ITREAP[index-1])).

Note:
- (FUNCALL TEST +OP-IDENTITY+) must be true.
- TEST must be monotone in the target range.
"
  (declare ((integer 0 #.most-positive-fixnum) start))
  (assert (funcall test +op-identity+))
  (multiple-value-bind (itreap-prefix itreap)
      (if (zerop start)
          (values nil itreap)
          (itreap-split itreap start))
    (labels
        ((recur (itreap offset prev-sum)
           (declare ((integer 0 #.most-positive-fixnum) offset)
                    #+sbcl (values (integer 0 #.most-positive-fixnum)))
           (unless itreap
             (return-from recur offset))
           (force-down itreap)
           (let ((sum prev-sum))
             (prog1
                 (cond ((not (funcall test (setq sum (op sum (itreap-accumulator (%itreap-left itreap))))))
                        (recur (%itreap-left itreap) offset prev-sum))
                       ((not (funcall test (setq sum (op sum (%itreap-value itreap)))))
                        (+ offset (itreap-count (%itreap-left itreap))))
                       (t
                        (recur (%itreap-right itreap)
                               (+ offset (itreap-count (%itreap-left itreap)) 1)
                               sum)))
               (force-up itreap)))))
      (prog1 (+ start (recur itreap 0 +op-identity+))
        (itreap-merge itreap-prefix itreap)))))

(declaim (inline itreap-fold-bisect-from-end))
(defun itreap-fold-bisect-from-end (itreap test &optional end)
  "Returns the smallest index that satisfies (FUNCALL TEST (OP ITREAP[index]
  ITREAP[index+1] ... ITREAP[END-1])).

Note:
- (FUNCALL TEST +OP-IDENTITY+) must be true.
- TEST must be monotone in the target range.
"
  (declare ((or null (integer 0 #.most-positive-fixnum)) end))
  (assert (funcall test +op-identity+))
  (multiple-value-bind (itreap itreap-suffix)
      (if end
          (itreap-split itreap end)
          (values itreap nil))
    (labels
        ((recur (itreap offset prev-sum)
           (declare ((integer 0 #.most-positive-fixnum) offset)
                    #+sbcl (values (integer 0 #.most-positive-fixnum)))
           (unless itreap
             (return-from recur offset))
           (force-down itreap)
           (let ((sum prev-sum))
             (prog1
                 (cond ((not (funcall test (setq sum (op (itreap-accumulator (%itreap-right itreap)) sum))))
                        (recur (%itreap-right itreap) offset prev-sum))
                       ((not (funcall test (setq sum (op (%itreap-value itreap) sum))))
                        (+ offset (itreap-count (%itreap-right itreap))))
                       (t
                        (recur (%itreap-left itreap)
                               (+ offset (itreap-count (%itreap-right itreap)) 1)
                               sum)))
               (force-up itreap)))))
      (prog1 (- (or end (itreap-count itreap))
                (recur itreap 0 +op-identity+))
        (itreap-merge itreap itreap-suffix)))))

(declaim (inline itreap-update))
(defun itreap-update (itreap operand l r)
  "Updates ITRAP by ITREAP[i] := (OP ITREAP[i] OPERAND) for all i in [l, r)"
  (declare ((integer 0 #.most-positive-fixnum) l r))
  (unless (<= l r (itreap-count itreap))
    (error 'invalid-itreap-index-error :itreap itreap :index (cons l r)))
  (labels
      ((recur (itreap l r)
         (declare ((integer 0 #.most-positive-fixnum) l r))
         (when itreap
           (if (and (zerop l) (= r (%itreap-count itreap)))
               (progn
                 (setf (%itreap-lazy itreap)
                       (updater-op (%itreap-lazy itreap) operand))
                 (force-down itreap))
               (let ((left-count (itreap-count (%itreap-left itreap))))
                 (force-down itreap)
                 (if (<= l left-count)
                     (if (< left-count r)
                         ;; LEFT-COUNT is in [L, R)
                         (progn
                           (recur (%itreap-left itreap) l (min r left-count))
                           (setf (%itreap-value itreap)
                                 (modifier-op (%itreap-value itreap) operand 1))
                           (recur (%itreap-right itreap) 0 (- r left-count 1)))
                         ;; LEFT-COUNT is in [R, END)
                         (recur (%itreap-left itreap) l (min r left-count)))
                     ;; LEFT-COUNT is in [0, L)
                     (recur (%itreap-right itreap) (- l left-count 1) (- r left-count 1)))))
           (force-up itreap))))
    (recur itreap l r)
    itreap))

(declaim (inline itreap-reverse))
(defun itreap-reverse (itreap l r)
  "Destructively reverses the order of the interval [L, R) in O(log(n)) time.

You cannot rely on the side effect. Use the returned value."
  (declare ((integer 0 #.most-positive-fixnum) l r))
  (unless (<= l r (itreap-count itreap))
    (error 'invalid-itreap-index-error :itreap itreap :index (cons l r)))
  (multiple-value-bind (itreap-0-l itreap-l-n)
      (itreap-split itreap l)
    (multiple-value-bind (itreap-l-r itreap-r-n)
        (itreap-split itreap-l-n (- r l))
      (setf (%itreap-reversed itreap-l-r) (not (%itreap-reversed itreap-l-r)))
      (itreap-merge itreap-0-l (itreap-merge itreap-l-r itreap-r-n)))))

;;;
;;; Utilities for sorted treap
;;;

(declaim (inline itreap-bisect-left)
         (ftype (function * (values (integer 0 #.most-positive-fixnum) &optional)) itreap-bisect-left))
(defun itreap-bisect-left (itreap threshold order)
  "Takes a **sorted** treap and returns the smallest index that satisfies
ITREAP[index] >= THRESHOLD, where >= is the complement of ORDER. Returns the
size of ITREAP if ITREAP[length-1] < THRESHOLD. The time complexity is
O(log(n))."
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

(declaim (inline itreap-bisect-right)
         (ftype (function * (values (integer 0 #.most-positive-fixnum) &optional)) itreap-bisect-right))
(defun itreap-bisect-right (itreap threshold order)
  "Takes a **sorted** treap and returns the smallest index that satisfies
THRESHOLD < ITREAP[index], where < is ORDER. Returns the size of ITREAP if
ITREAP[length-1] <= THRESHOLD. The time complexity is O(log(n))."
  (labels ((recur (count itreap)
             (declare ((integer 0 #.most-positive-fixnum) count))
             (cond ((null itreap) nil)
                   ((funcall order threshold (%itreap-value itreap))
                    (let ((left-count (- count (itreap-count (%itreap-right itreap)) 1)))
                      (or (recur left-count (%itreap-left itreap))
                          left-count)))
                   (t
                    (recur count (%itreap-right itreap))))))
    (or (recur (itreap-count itreap) itreap)
        (itreap-count itreap))))

(declaim (inline itreap-insort))
(defun itreap-insort (itreap obj order)
  "Does insertion to the sorted treap."
  (let ((pos (itreap-bisect-left itreap obj order)))
    (itreap-insert itreap pos obj)))
