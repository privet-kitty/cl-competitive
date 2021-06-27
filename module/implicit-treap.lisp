(defpackage :cp/implicit-treap
  (:use :cl)
  (:export #:itreap #:itreap-p #:itreap-count #:itreap-accumulator
           #:make-itreap #:invalid-itreap-index-error #:itreap-ref
           #:itreap-split #:itreap-merge #:itreap-insert #:itreap-delete
           #:itreap-push #:itreap-pop #:itreap-map #:do-itreap
           #:itreap-fold #:itreap-max-right #:itreap-min-left
           #:itreap-update #:itreap-reverse
           #:itreap-bisect-left #:itreap-bisect-right #:itreap-insort)
  (:documentation "Provides implicit treap.

NOTE: an empty treap is NIL.
TODO: abstraction"))
(in-package :cp/implicit-treap)

(declaim (inline op))
(defun op (a b)
  "Is a binary operator comprising a monoid."
  (min a b))

(defconstant +op-identity+ most-positive-fixnum
  "identity element w.r.t. OP")

(declaim (inline updater-op))
(defun updater-op (lazy x)
  "Is the operator to compute and update LAZY value. LAZY is the current LAZY
value and X is an operand."
  (+ lazy x))

(defconstant +updater-identity+ 0
  "identity element w.r.t. UPDATER-OP")

(declaim (inline modifier-op))
(defun modifier-op (acc lazy size)
  "Is the operator to update ACCUMULATOR (and VALUE) based on LAZY value. ACC is
the current ACCUMULATOR value and LAZY is the LAZY value. SIZE is the length of
the target interval."
  (declare (ignorable size))
  (+ acc lazy))

(deftype index () '(integer 0 #.(floor most-positive-fixnum 2)))
(defstruct (itreap (:constructor %make-itreap (value priority &key left right (count 1) (accumulator value) (lazy +updater-identity+) reversed))
                   (:copier nil)
                   (:conc-name %itreap-))
  (value +op-identity+ :type fixnum)
  (accumulator +op-identity+ :type fixnum)
  (lazy +updater-identity+ :type fixnum)
  (reversed nil :type boolean)
  (priority 0 :type (mod #.most-positive-fixnum))
  (count 1 :type index) ; size of (sub)treap
  (left nil :type (or null itreap))
  (right nil :type (or null itreap)))

(declaim (inline itreap-count))
(defun itreap-count (itreap)
  "Returns the number of the elements of ITREAP."
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
    (let ((left (%itreap-left itreap)))
      (when left
        (setf (%itreap-lazy left)
              (updater-op (%itreap-lazy left) (%itreap-lazy itreap)))
        (setf (%itreap-accumulator left)
              (modifier-op (%itreap-accumulator left)
                           (%itreap-lazy itreap)
                           (%itreap-count left)))))
    (let ((right (%itreap-right itreap)))
      (when right
        (setf (%itreap-lazy right)
              (updater-op (%itreap-lazy right) (%itreap-lazy itreap)))
        (setf (%itreap-accumulator right)
              (modifier-op (%itreap-accumulator right)
                           (%itreap-lazy itreap)
                           (%itreap-count right)))))
    (setf (%itreap-value itreap)
          (modifier-op (%itreap-value itreap)
                       (%itreap-lazy itreap)
                       1))
    (setf (%itreap-lazy itreap) +updater-identity+)))

(defun %heapify (node)
  "Makes it max-heap w.r.t. priorities by swapping the priorities of the whole
treap."
  (declare (optimize (speed 3) (safety 0)))
  (when node
    (let ((high-priority-node node))
      (when (and (%itreap-left node)
                 (> (%itreap-priority (%itreap-left node))
                    (%itreap-priority high-priority-node)))
        (setq high-priority-node (%itreap-left node)))
      (when (and (%itreap-right node)
                 (> (%itreap-priority (%itreap-right node))
                    (%itreap-priority high-priority-node)))
        (setq high-priority-node (%itreap-right node)))
      (unless (eql high-priority-node node)
        (rotatef (%itreap-priority high-priority-node)
                 (%itreap-priority node))
        (%heapify high-priority-node)))))

(declaim (inline make-itreap))
(defun make-itreap (size &key (initial-element nil supplied-p) initial-contents)
  "Makes a treap of SIZE in O(SIZE) time. Its values are filled with the
INITIAL-ELEMENT (or identity element) unless INITIAL-CONTENTS are supplied."
  (declare ((or null vector) initial-contents))
  (labels ((build (l r)
             (declare (index l r))
             (if (= l r)
                 nil
                 (let* ((mid (ash (+ l r) -1))
                        (node (%make-itreap (cond (initial-contents
                                                   (aref initial-contents mid))
                                                  (supplied-p
                                                   initial-element)
                                                  (t +op-identity+))
                                            (random (+ 1 most-positive-fixnum)))))
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
  "Destructively splits ITREAP at INDEX and returns two treaps (in ascending
order)."
  (declare (optimize (speed 3))
           (index index))
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
  "Destructively concatenates two ITREAPs. Note that this `merge' is different
from CL:MERGE and rather close to CL:CONCATENATE."
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
  "Destructively inserts OBJ into ITREAP at INDEX and returns the resultant treap.

You cannot rely on the side effect. Use the returned value."
  (declare (optimize (speed 3))
           ((or null itreap) itreap)
           (index index))
  (unless (<= index (itreap-count itreap))
    (error 'invalid-itreap-index-error :itreap itreap :index index))
  (let ((node (%make-itreap obj (random most-positive-fixnum))))
    (labels ((recur (itreap ikey)
               (declare (index ikey))
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
           (index index))
  (unless (< index (itreap-count itreap))
    (error 'invalid-itreap-index-error :itreap itreap :index index))
  (labels ((recur (itreap ikey)
             (declare (index ikey))
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

(defmacro treap-push (key treap &optional (order '#'<) &environment env)
  "Pushes a KEY to TREAP."
  (multiple-value-bind (temps vals stores setter getter)
      (get-setf-expansion treap env)
    `(let* (,@(mapcar #'list temps vals)
            (,(car stores) (treap-insert ,key ,getter :order ,order))
            ,@(cdr stores))
       ,setter)))

(defmacro treap-pop (key treap &optional (order '#'<) &environment env)
  "Deletes a KEY from TREAP."
  (multiple-value-bind (temps vals stores setter getter)
      (get-setf-expansion treap env)
    `(let* (,@(mapcar #'list temps vals)
            (,(car stores) (treap-delete ,key ,getter :order ,order))
            ,@(cdr stores))
       ,setter)))

(defmacro itreap-push (itreap pos obj &environment env)
  "Pushes OBJ to ITREAP at POS."
  (multiple-value-bind (temps vals stores setter getter)
      (get-setf-expansion itreap env)
    `(let* (,@(mapcar #'list temps vals)
            (,(car stores) (itreap-insert ,getter ,pos ,obj))
            ,@(cdr stores))
       ,setter)))

(defmacro itreap-pop (itreap pos &environment env)
  "Returns the object at POS and deletes it."
  (multiple-value-bind (temps vals stores setter getter)
      (get-setf-expansion itreap env)
    (let ((source (gensym "SOURCE"))
          (ret (gensym "RET"))
          (p (gensym "POS")))
      `(let* (,@(mapcar #'list temps vals)
              (,source ,getter)
              (,p ,pos)
              (,ret (itreap-ref ,source ,p))
              (,(car stores) (itreap-delete ,source ,p))
              ,@(cdr stores))
         ,setter
         ,ret))))

(declaim (inline itreap-map))
(defun itreap-map (itreap function &optional (start 0) end)
  "Successively applies FUNCTION to ITREAP[START], ..., ITREAP[END-1]."
  (declare (function function)
           (index start)
           ((or null index) end))
  (unless end
    (setq end (itreap-count itreap)))
  (unless (<= start end (itreap-count itreap))
    (error 'invalid-itreap-index-error :itreap itreap :index (cons start end)))
  (labels ((recur (itreap l r)
             (declare (index l r))
             (when (< l r)
               (force-down itreap)
               (force-up itreap)
               (let ((lcount (itreap-count (%itreap-left itreap))))
                 (when (< l lcount)
                   (recur (%itreap-left itreap) l (min r lcount)))
                 (when (and (<= l lcount) (< lcount r))
                   (funcall function (%itreap-value itreap)))
                 (when (< (+ lcount 1) r)
                   (recur (%itreap-right itreap) (max 0 (- l lcount 1)) (- r lcount 1)))))))
    (recur itreap start end)))

(defmethod print-object ((object itreap) stream)
  (print-unreadable-object (object stream :type t)
    (let ((init t))
      (itreap-map object
                  (lambda (x)
                    (if init
                        (setq init nil)
                        (write-char #\  stream))
                    (write x :stream stream))))))

(defmacro do-itreap ((var itreap &optional result) &body body)
  "Successively binds ITREAP[0], ..., ITREAP[SIZE-1] to VAR and executes BODY
each time."
  `(block nil
     (itreap-map ,itreap (lambda (,var) ,@body))
     ,result))

(defun itreap (&rest args)
  ;; NOTE: This function takes O(nlog(n)) time. Use MAKE-ITREAP for efficiency.
  (labels ((recur (list position itreap)
             (declare (index position))
             (if (null list)
                 itreap
                 (recur (cdr list)
                        (+ 1 position)
                        (itreap-insert itreap position (car list))))))
    (recur args 0 nil)))

(declaim (inline itreap-ref))
(defun itreap-ref (itreap index)
  "Returns the element ITREAP[INDEX]."
  (declare (index index))
  (unless (< index (itreap-count itreap))
    (error 'invalid-itreap-index-error :itreap itreap :index index))
  (labels ((%ref (itreap index)
             (declare (index index))
             (force-down itreap)
             (force-up itreap)
             (let ((left-count (itreap-count (%itreap-left itreap))))
               (cond ((< index left-count)
                      (%ref (%itreap-left itreap) index))
                     ((> index left-count)
                      (%ref (%itreap-right itreap) (- index left-count 1)))
                     (t (%itreap-value itreap))))))
    (%ref itreap index)))

(declaim (inline (setf itreap-ref)))
(defun (setf itreap-ref) (new-value itreap index)
  "Sets ITREAP[INDEX] to the given value."
  (declare (index index))
  (unless (< index (itreap-count itreap))
    (error 'invalid-itreap-index-error :itreap itreap :index index))
  (labels ((%set (itreap index)
             (declare (index index))
             (force-down itreap)
             (let ((left-count (itreap-count (%itreap-left itreap))))
               (cond ((< index left-count)
                      (%set (%itreap-left itreap) index))
                     ((> index left-count)
                      (%set (%itreap-right itreap) (- index left-count 1)))
                     (t (setf (%itreap-value itreap) new-value))))
             (force-up itreap)))
    (%set itreap index)
    new-value))

(declaim (inline itreap-fold))
(defun itreap-fold (itreap l r)
  "Returns the `sum' (w.r.t. OP) of the range ITREAP[L, R)."
  (declare (index l r))
  (unless (<= l r (itreap-count itreap))
    (error 'invalid-itreap-index-error :itreap itreap :index (cons l r)))
  (labels
      ((recur (itreap l r)
         (declare (index l r))
         (unless itreap
           (return-from recur +op-identity+))
         (force-down itreap)
         (force-up itreap)
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
                   (recur (%itreap-right itreap) (- l left-count 1) (- r left-count 1)))))))
    (recur itreap l r)))

(declaim (ftype (function * (values index &optional)) itreap-max-right)
         (inline itreap-max-right))
(defun itreap-max-right (itreap test &optional (start 0))
  "Returns the largest index that satisfies (FUNCALL TEST (OP ITREAP[START]
ITREAP[START+1] ... ITREAP[index-1])).

Note:
- (FUNCALL TEST +OP-IDENTITY+) must be true.
- TEST must be monotone in the target range.
"
  (declare (index start))
  (assert (funcall test +op-identity+))
  (when (< (itreap-count itreap) start)
    (error 'invalid-itreap-index-error :index start :itreap itreap))
  (labels
      ((fold (itreap offset)
         (declare (index offset))
         (unless itreap
           (return-from fold +op-identity+))
         (force-down itreap)
         (force-up itreap)
         (let ((lcount (+ offset (itreap-count (%itreap-left itreap)))))
           (declare (index lcount))
           (if (< lcount start)
               (fold (%itreap-right itreap) (+ lcount 1))
               (let ((sum (fold (%itreap-left itreap) offset)))
                 (cond ((not (funcall test (setq sum (op sum (%itreap-value itreap)))))
                        (return-from itreap-max-right lcount))
                       ((funcall test (op sum (itreap-accumulator (%itreap-right itreap))))
                        sum)
                       (t (search-subtree (%itreap-right itreap) (+ lcount 1) sum)))))))
       (search-subtree (itreap offset prev-sum)
         (declare (index offset))
         (unless itreap
           (return-from itreap-max-right offset))
         (force-down itreap)
         (force-up itreap)
         (let ((sum (op prev-sum (itreap-accumulator (%itreap-left itreap)))))
           (cond ((not (funcall test sum))
                  (search-subtree (%itreap-left itreap) offset prev-sum))
                 ((not (funcall test (setq sum (op sum (%itreap-value itreap)))))
                  (return-from itreap-max-right
                    (+ offset (itreap-count (%itreap-left itreap)))))
                 (t
                  (search-subtree (%itreap-right itreap)
                                  (+ offset (itreap-count (%itreap-left itreap)) 1)
                                  sum))))))
    (if (zerop start)
        (search-subtree itreap 0 +op-identity+)
        (progn (fold itreap 0)
               (itreap-count itreap)))))

(declaim (ftype (function * (values index &optional)) itreap-min-left)
         (inline itreap-min-left))
(defun itreap-min-left (itreap test &optional end)
  "Returns the smallest index that satisfies (FUNCALL TEST (OP ITREAP[index]
  ITREAP[index+1] ... ITREAP[END-1])).

Note:
- (FUNCALL TEST +OP-IDENTITY+) must be true.
- TEST must be monotone in the target range.
"
  (declare ((or null index) end))
  (assert (funcall test +op-identity+))
  (when (and end (< (itreap-count itreap) end))
    (error 'invalid-itreap-index-error :index end :itreap itreap))
  (let* ((n (itreap-count itreap))
         (n-end (- n (or end n))))
    (declare (index n-end))
    (labels
        ((fold (itreap offset)
           (declare (index offset))
           (unless itreap
             (return-from fold +op-identity+))
           (force-down itreap)
           (force-up itreap)
           (let ((rcount (+ offset (itreap-count (%itreap-right itreap)))))
             (declare (index rcount))
             (if (< rcount n-end)
                 (fold (%itreap-left itreap) (+ rcount 1))
                 (let ((sum (fold (%itreap-right itreap) offset)))
                   (cond ((not (funcall test (setq sum (op (%itreap-value itreap) sum))))
                          (return-from itreap-min-left (- n rcount)))
                         ((funcall test (op (itreap-accumulator (%itreap-left itreap)) sum))
                          sum)
                         (t (search-subtree (%itreap-left itreap) (+ rcount 1) sum)))))))
         (search-subtree (itreap offset prev-sum)
           (declare (index offset))
           (unless itreap
             (return-from itreap-min-left (- n offset)))
           (force-down itreap)
           (force-up itreap)
           (let ((sum (op (itreap-accumulator (%itreap-right itreap)) prev-sum)))
             (cond ((not (funcall test sum))
                    (search-subtree (%itreap-right itreap) offset prev-sum))
                   ((not (funcall test (setq sum (op (%itreap-value itreap) sum))))
                    (return-from itreap-min-left
                      (- n (+ offset (itreap-count (%itreap-right itreap))))))
                   (t
                    (search-subtree (%itreap-left itreap)
                                    (+ offset (itreap-count (%itreap-right itreap)) 1)
                                    sum))))))
      (if (zerop n-end)
          (search-subtree itreap 0 +op-identity+)
          (progn (fold itreap 0)
                 0)))))

(declaim (inline itreap-update))
(defun itreap-update (itreap operand l r)
  "Updates ITRAP by ITREAP[i] := (OP ITREAP[i] OPERAND) for all i in [l, r)"
  (declare (index l r))
  (unless (<= l r (itreap-count itreap))
    (error 'invalid-itreap-index-error :itreap itreap :index (cons l r)))
  (labels
      ((recur (itreap l r)
         (declare (index l r))
         (when itreap
           (if (and (zerop l) (= r (%itreap-count itreap)))
               (progn
                 (setf (%itreap-lazy itreap)
                       (updater-op (%itreap-lazy itreap) operand))
                 (force-down itreap))
               (progn
                 (force-down itreap)
                 (let ((left-count (itreap-count (%itreap-left itreap))))
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
                       (recur (%itreap-right itreap)
                              (- l left-count 1)
                              (- r left-count 1))))))
           (force-up itreap))))
    (recur itreap l r)
    itreap))

(declaim (inline itreap-reverse))
(defun itreap-reverse (itreap l r)
  "Destructively reverses the order of the interval [L, R) in O(log(n)) time.

You cannot rely on the side effect. Use the returned value."
  (declare (index l r))
  (unless (<= l r (itreap-count itreap))
    (error 'invalid-itreap-index-error :itreap itreap :index (cons l r)))
  (when (= l r)
    (return-from itreap-reverse itreap))
  (multiple-value-bind (itreap-0-l itreap-l-n)
      (itreap-split itreap l)
    (multiple-value-bind (itreap-l-r itreap-r-n)
        (itreap-split itreap-l-n (- r l))
      (setf (%itreap-reversed itreap-l-r) (not (%itreap-reversed itreap-l-r)))
      (itreap-merge itreap-0-l (itreap-merge itreap-l-r itreap-r-n)))))

;;;
;;; Below are utilities for treap whose values are sorted w.r.t. some order
;;;

(declaim (inline itreap-bisect-left)
         (ftype (function * (values index &optional)) itreap-bisect-left))
(defun itreap-bisect-left (itreap value order &key (key #'identity))
  "Takes a **sorted** treap and returns the smallest index that satisfies
ITREAP[index] >= VALUE, where >= is the complement of ORDER. In other words,
this function returns a leftmost index at which value can be inserted with
keeping the order. Returns the size of ITREAP if ITREAP[length-1] <
VALUE. The time complexity is O(log(n))."
  (labels ((recur (count itreap)
             (declare (index count))
             (cond ((null itreap) nil)
                   ((funcall order (funcall key (%itreap-value itreap)) value)
                    (recur count (%itreap-right itreap)))
                   (t
                    (let ((left-count (- count (itreap-count (%itreap-right itreap)) 1)))
                      (or (recur left-count (%itreap-left itreap))
                          left-count))))))
    (or (recur (itreap-count itreap) itreap)
        (itreap-count itreap))))

(declaim (inline itreap-bisect-right)
         (ftype (function * (values index &optional)) itreap-bisect-right))
(defun itreap-bisect-right (itreap value order &key (key #'identity))
  "Takes a **sorted** treap and returns the smallest index that satisfies
VALUE < ITREAP[index], where < is ORDER. In other words, this function
returns a rightmost index at which VALUE can be inserted with keeping the
order. Returns the size of ITREAP if ITREAP[length-1] <= VALUE. The time
complexity is O(log(n))."
  (labels ((recur (count itreap)
             (declare (index count))
             (cond ((null itreap) nil)
                   ((funcall order value (funcall key (%itreap-value itreap)))
                    (let ((left-count (- count (itreap-count (%itreap-right itreap)) 1)))
                      (or (recur left-count (%itreap-left itreap))
                          left-count)))
                   (t
                    (recur count (%itreap-right itreap))))))
    (or (recur (itreap-count itreap) itreap)
        (itreap-count itreap))))

(declaim (inline itreap-insort))
(defun itreap-insort (itreap obj order)
  "Does insertion to the sorted treap with keeping the order. You cannot rely on
the side effect. Use the returned value."
  (let ((pos (itreap-bisect-left itreap obj order)))
    (itreap-insert itreap pos obj)))
