(setf *print-circle* t)

(defconstant +op-identity+ 0)
(declaim (inline op))
(defun op (x y)
  (+ x y))

;; Treap with explicit key
(defstruct (treap (:constructor make-treap (key priority value accumulator &key left right (count 1)))
                  (:copier nil)
                  (:conc-name %treap-))
  (key 0 :type fixnum)
  (value nil :type fixnum)
  (accumulator nil :type fixnum)
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

(declaim (inline treap-accumulator))
(defun treap-accumulator (treap)
  (declare ((or null treap) treap))
  (if (null treap)
      +op-identity+
      (%treap-accumulator treap)))

(declaim (inline update-count))
(defun update-count (treap)
  (declare (treap treap))
  (setf (%treap-count treap)
        (+ 1
           (treap-count (%treap-left treap))
           (treap-count (%treap-right treap)))))

(declaim (inline update-accumulator))
(defun update-accumulator (treap)
  (declare (treap treap))
  (setf (%treap-accumulator treap)
        (if (%treap-left treap)
            (if (%treap-right treap)
                (op (op (%treap-accumulator (%treap-left treap))
                        (%treap-value treap))
                    (%treap-accumulator (%treap-right treap)))
                (op (%treap-accumulator (%treap-left treap))
                    (%treap-value treap)))
            (if (%treap-right treap)
                (op (%treap-value treap)
                    (%treap-accumulator (%treap-right treap)))
                (%treap-value treap)))))

(declaim (inline force-self))
(defun force-self (treap)
  (declare (treap treap))
  (update-count treap)
  (update-accumulator treap))

(defun treap-find (key treap &key (test #'<))
  "Finds the key that satisfies (and (not (funcall test key (%treap-key
sub-treap))) (not (funcall test (%treap-key sub-treap) key))) and returns
KEY. Returns NIL if KEY is not contained."
  (declare (function test)
           ((or null treap) treap))
  (cond ((null treap) nil)
        ((funcall test key (%treap-key treap))
         (treap-find key (%treap-left treap) :test test))
        ((funcall test (%treap-key treap) key)
         (treap-find key (%treap-right treap) :test test))
        (t key)))

(defun treap-bisect-left (threshold treap &key (test #'<))
  "Returns the smallest index and the corresponding key that satisfies
KEY[index] >= THRESHOLD. Returns the size of TREAP and THRESHOLD if KEY[size-1]
< THRESHOLD."
  (declare (function test))
  (labels ((recur (count treap)
             (declare ((integer 0 #.most-positive-fixnum) count))
             (cond ((null treap) (values nil nil))
                   ((funcall test (%treap-key treap) threshold)
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
          (values (treap-count treap) threshold)))))

(declaim (ftype (function * (values (or null treap) (or null treap) &optional)) treap-split))
(defun treap-split (key treap &key (test #'<))
  "Destructively splits the TREAP with reference to KEY and returns two treaps,
the smaller sub-treap (< KEY) and the larger one (>= KEY)."
  (declare (function test)
           ((or null treap) treap))
  (cond ((null treap)
         (values nil nil))
        ((funcall test (%treap-key treap) key)
         (multiple-value-bind (left right)
             (treap-split key (%treap-right treap) :test test)
           (setf (%treap-right treap) left)
           (force-self treap)
           (values treap right)))
        (t
         (multiple-value-bind (left right)
             (treap-split key (%treap-left treap) :test test)
           (setf (%treap-left treap) right)
           (force-self treap)
           (values left treap)))))

(declaim (inline treap-insert))
(defun treap-insert (key value treap &key (test #'<))
  "Destructively inserts KEY into TREAP and returns the result treap. You cannot
rely on the side effect. Use the returned value.

The behavior is undefined when duplicated keys are inserted."
  (declare ((or null treap) treap)
           (function test))
  (labels ((recur (node treap)
             (declare (treap node))
             (cond ((null treap) node)
                   ((> (%treap-priority node) (%treap-priority treap))
                    (setf (values (%treap-left node) (%treap-right node))
                          (treap-split (%treap-key node) treap :test test))
                    (force-self node)
                    node)
                   (t
                    (if (funcall test (%treap-key node) (%treap-key treap))
                        (setf (%treap-left treap)
                              (recur node (%treap-left treap)))
                        (setf (%treap-right treap)
                              (recur node (%treap-right treap))))
                    (force-self treap)
                    treap))))
    (recur (make-treap key (random most-positive-fixnum) value value) treap)))

(declaim (inline treap-ensure-key))
(defun treap-ensure-key (key value treap &key (test #'<) if-exists)
  "IF-EXISTS := nil | function

Ensures that TREAP contains KEY and assigns VALUE to it If IF-EXISTS is null. If
IF-EXISTS is function and TREAP contains KEY, TREAP-ENSURE-KEY updates the value
by the function instead of overwriting it with VALUE."
  (declare (function test)
           ((or null treap) treap))
  (labels ((find-and-update (treap)
             ;; Updates value and returns T if KEY exists
             (cond ((null treap) nil)
                   ((funcall test key (%treap-key treap))
                    (when (find-and-update (%treap-left treap))
                      (force-self treap)
                      t))
                   ((funcall test (%treap-key treap) key)
                    (when (find-and-update (%treap-right treap))
                      (force-self treap)
                      t))
                   (t (setf (%treap-value treap)
                            (if if-exists
                                (funcall if-exists (%treap-value treap))
                                value))
                      (force-self treap)
                      t))))
    (if (find-and-update treap)
        treap
        (treap-insert key value treap :test test))))

(defun treap-merge (left right)
  "Destructively merges two treaps. Assumes that all keys of LEFT are smaller
 (or larger, depending on the order) than those of RIGHT."
  (declare ((or null treap) left right))
  (cond ((null left) right)
        ((null right) left)
        ((> (%treap-priority left) (%treap-priority right))
         (setf (%treap-right left)
               (treap-merge (%treap-right left) right))
         (force-self left)
         left)
        (t
         (setf (%treap-left right)
               (treap-merge left (%treap-left right)))
         (force-self right)
         right)))

(defun treap-delete (key treap &key (test #'<))
  "Destructively deletes the KEY in TREAP and returns the result treap. Returns
the unmodified TREAP If KEY doesn't exist. You cannot rely on the side
effect. Use the returned value.

 (Note that this function deletes at most one node even if duplicated keys
exist.)"
  (declare ((or null treap) treap)
           (function test))
  (cond ((null treap) nil)
        ((funcall test key (%treap-key treap))
         (setf (%treap-left treap)
               (treap-delete key (%treap-left treap) :test test))
         (force-self treap)
         treap)
        ((funcall test (%treap-key treap) key)
         (setf (%treap-right treap)
               (treap-delete key (%treap-right treap) :test test))
         (force-self treap)
         treap)
        (t
         (treap-merge (%treap-left treap) (%treap-right treap)))))

(defun treap-map (function treap)
  "Successively applies FUNCTION to TREAP[0], ..., TREAP[SIZE-1]. FUNCTION must
take two arguments: KEY and VALUE."
  (declare (function function))
  (when treap
    (treap-map function (%treap-left treap))
    (funcall function (%treap-key treap) (%treap-value treap))
    (treap-map function (%treap-right treap))))

(defmethod print-object ((object treap) stream)
  (print-unreadable-object (object stream :type t)
    (let ((init t))
      (treap-map (lambda (key value)
                   (if init
                       (setf init nil)
                       (write-char #\  stream))
                   (format stream "<~A . ~A>" key value))
                 object))))

(defmacro do-treap ((key-var value-var treap &optional result) &body body)
  "Successively binds the key and value of INODE[0], ..., INODE[SIZE-1] to
KEY-VAR and VALUE-VAR and executes BODY."
  `(block nil
     (treap-map (lambda (,key-var ,value-var) ,@body) ,treap)
     ,result))

(define-condition invalid-treap-index-error (type-error)
  ((treap :initarg :treap :reader invalid-treap-index-error-treap)
   (index :initarg :index :reader invalid-treap-index-error-index))
  (:report
   (lambda (condition stream)
     (format stream "Invalid index ~W for treap ~S."
             (invalid-treap-index-error-index condition)
             (invalid-treap-index-error-treap condition)))))

(declaim (inline treap-ref))
(defun treap-ref (treap index)
  "Returns the key and value corresponding to the INDEX."
  (declare ((or null treap) treap)
           ((integer 0 #.most-positive-fixnum) index))
  (when (>= index (treap-count treap))
    (error (make-condition 'invalid-treap-index-error :treap treap :index index)))
  (labels ((%ref (treap index)
             (declare ((integer 0 #.most-positive-fixnum) index))
             (let ((left-count (treap-count (%treap-left treap))))
               (cond ((< index left-count)
                      (%ref (%treap-left treap) index))
                     ((> index left-count)
                      (%ref (%treap-right treap) (- index left-count 1)))
                     (t (values (%treap-key treap) (%treap-value treap)))))))
    (%ref treap index)))

;; FIXME: might be problematic when two priorities collide.
(declaim (inline treap-query))
(defun treap-query (treap &key left right (test #'<))
  "Queries the sum of the half-open interval specified by the keys: [LEFT,
RIGHT). If LEFT (RIGHT) is not given, it is assumed to be -inf (+inf)."
  (if (null left)
      (if (null right)
          (treap-accumulator treap)
          (multiple-value-bind (treap-0-r treap-r-n)
              (treap-split right treap :test test)
            (prog1 (treap-accumulator treap-0-r)
              (treap-merge treap-0-r treap-r-n))))
      (if (null right)
          (multiple-value-bind (treap-0-l treap-l-n)
              (treap-split left treap :test test)
            (prog1 (treap-accumulator treap-l-n)
              (treap-merge treap-0-l treap-l-n)))
          (progn
            (assert (not (funcall test right left)))
            (multiple-value-bind (treap-0-l treap-l-n)
                (treap-split left treap :test test)
              (multiple-value-bind (treap-l-r treap-r-n)
                  (treap-split right treap-l-n :test test)
                (prog1 (treap-accumulator treap-l-r)
                  (treap-merge treap-0-l (treap-merge treap-l-r treap-r-n)))))))))

(defun copy-treap (treap)
  "For development. Recursively copies the whole TREAP."
  (declare ((or null treap) treap))
  (if (null treap)
      nil
      (make-treap (%treap-key treap)
                  (%treap-priority treap)
                  (%treap-value treap)
                  (%treap-accumulator treap)
                  :left (copy-treap (%treap-left treap))
                  :right (copy-treap (%treap-right treap))
                  :count (%treap-count treap))))

;; Test
;; (let ((treap1 (make-treap 50 15 :count 5))
;;       (treap2 (make-treap 100 11 :count 3)))
;;   (setf (%treap-left treap1) (make-treap 30 5 :count 3))
;;   (setf (%treap-left (%treap-left treap1)) (make-treap 20 2 :count 1))
;;   (setf (%treap-right (%treap-left treap1)) (make-treap 40 4 :count 1))
;;   (setf (%treap-right treap1) (make-treap 70 10 :count 1))
;;   (setf (%treap-right treap2) (make-treap 200 3 :count 1))
;;   (setf (%treap-left treap2) (make-treap 99 5 :count 1))
;;   ;; copy-treap
;;   (assert (equalp treap1 (copy-treap treap1)))
;;   (assert (not (eql treap1 (copy-treap treap1))))
;;   ;; split and merge
;;   (let ((treap (treap-merge (copy-treap treap1) (copy-treap treap2))))
;;     (assert (= 8 (%treap-count treap)))
;;     (multiple-value-bind (left right) (treap-split 80 (copy-treap treap))
;;       (assert (= 5 (%treap-count left)))
;;       (assert (= 3 (%treap-count right)))
;;       (assert (equalp treap (treap-merge left right)))))
;;   ;; find
;;   (assert (= 40 (treap-find 40 treap1)))
;;   (assert (null (treap-find 41 treap1)))
;;   ;; insert and delete
;;   (let ((inserted-treap1 (treap-insert 41 (copy-treap treap1))))
;;     (assert (= 41 (treap-find 41 inserted-treap1)))
;;     (let ((deleted-treap1 (treap-delete 41 inserted-treap1)))
;;       (assert (null (treap-find 41 deleted-treap1)))
;;       (assert (equalp treap1 deleted-treap1))
;;       (assert (equalp treap1 (treap-delete 41 deleted-treap1)))))
;;   (let ((treap (treap-merge treap1 treap2)))
;;     (assert (= 20 (treap-ref treap 0)))
;;     (assert (= 30 (treap-ref treap 1)))
;;     (assert (= 40 (treap-ref treap 2)))
;;     (assert (= 50 (treap-ref treap 3)))
;;     (assert (= 70 (treap-ref treap 4)))
;;     (assert (= 99 (treap-ref treap 5)))
;;     (assert (= 100 (treap-ref treap 6)))
;;     (assert (= 200 (treap-ref treap 7)))))

;; (multiple-value-bind (left right) (treap-split 5 (treap-insert 0 (treap-insert 10 (treap-insert 5 nil))))
;;   (assert (= 0 (%treap-key left)))
;;   (assert (null (%treap-left left)))
;;   (assert (null (%treap-right left)))
;;   (assert (or (typep (%treap-left right) 'treap)
;;               (typep (%treap-right right) 'treap))))

