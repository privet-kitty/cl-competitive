;; Not included in test script. Better to use ref-able-treap instead.
(setf *print-circle* t)

(defstruct (treap (:constructor make-treap (key priority &optional left right))
                  (:copier nil)
                  (:conc-name %treap-))
  key
  (priority 0 :type (integer 0 #.most-positive-fixnum))
  (left nil :type (or null treap))
  (right nil :type (or null treap)))

(defun treap-find (key treap &key (test #'<))
  "Searches the sub-treap of TREAP whose key satisfies (and (not (funcall test
key (%treap-key sub-treap))) (not (funcall test (%treap-key sub-treap) key))) and
returns KEY. Returns NIL if KEY is not contained."
  (declare (function test)
           ((or null treap) treap))
  (cond ((null treap) nil)
        ((funcall test key (%treap-key treap))
         (treap-find key (%treap-left treap) :test test))
        ((funcall test (%treap-key treap) key)
         (treap-find key (%treap-right treap) :test test))
        (t key)))

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
           (values treap right)))
        (t
         (multiple-value-bind (left right)
             (treap-split key (%treap-left treap) :test test)
           (setf (%treap-left treap) right)
           (values left treap)))))

(declaim (inline treap-insert))
(defun treap-insert (key treap &key (test #'<))
  "Destructively inserts KEY into TREAP and returns the resultant treap. You
cannot rely on the side effect. Use the returned value.

The behavior is undefined when duplicated keys are inserted."
  (declare ((or null treap) treap)
           (function test))
  (labels ((recurse (node treap)
             (declare (treap node))
             (cond ((null treap) node)
                   ((> (%treap-priority node) (%treap-priority treap))
                    (setf (values (%treap-left node) (%treap-right node))
                          (treap-split (%treap-key node) treap :test test))
                    node)
                   (t
                    (if (funcall test (%treap-key node) (%treap-key treap))
                        (setf (%treap-left treap)
                              (recurse node (%treap-left treap)))
                        (setf (%treap-right treap)
                              (recurse node (%treap-right treap))))
                    treap))))
    (recurse (make-treap key (random most-positive-fixnum)) treap)))

(defun treap-map (function treap)
  "Successively applies FUNCTION to each key of TREAP in the given
order. FUNCTION must take one argument."
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
                       (setf init nil)
                       (write-char #\  stream))
                   (write key :stream stream))
                 object))))

(defun treap-merge (left right)
  "Destructively merges two treaps. Assumes that all keys of LEFT are smaller (or larger,
depending on the order) than those of RIGHT."
  (declare ((or null treap) left right))
  (cond ((null left) right)
        ((null right) left)
        ((> (%treap-priority left) (%treap-priority right))
         (setf (%treap-right left)
               (treap-merge (%treap-right left) right))
         left)
        (t
         (setf (%treap-left right)
               (treap-merge left (%treap-left right)))
         right)))

(defun treap-delete (key treap &key (test #'<))
  "Destructively deletes the KEY in TREAP and returns the result treap. You
cannot rely on the side effect. Use the returned value."
  (declare ((or null treap) treap)
           (function test))
  (cond ((null treap) nil)
        ((funcall test key (%treap-key treap))
         (setf (%treap-left treap) (treap-delete key (%treap-left treap) :test test))
         treap)
        ((funcall test (%treap-key treap) key)
         (setf (%treap-right treap) (treap-delete key (%treap-right treap) :test test))
         treap)
        (t
         (treap-merge (%treap-left treap) (%treap-right treap)))))

;; (defun copy-treap (treap)
;;   "For development. Recursively copies the whole TREAP."
;;   (declare ((or null treap) treap))
;;   (if (null treap)
;;       nil
;;       (make-treap (%treap-key treap)
;;                   (%treap-priority treap)
;;                   (copy-treap (%treap-left treap))
;;                   (copy-treap (%treap-right treap)))))

;; Test
;; (let ((treap1 (make-treap 50 15))
;;       (treap2 (make-treap 100 11)))
;;   (setf (%treap-left treap1) (make-treap 30 5))
;;   (setf (%treap-left (%treap-left treap1)) (make-treap 20 2))
;;   (setf (%treap-right (%treap-left treap1)) (make-treap 40 4))
;;   (setf (%treap-right treap1) (make-treap 70 10))
;;   (setf (%treap-right treap2) (make-treap 200 3))
;;   (setf (%treap-left treap2) (make-treap 99 5))
;;   ;; copy-treap
;;   (assert (equalp treap1 (copy-treap treap1)))
;;   (assert (not (eql treap1 (copy-treap treap1))))
;;   ;; split and merge
;;   (let ((treap (treap-merge (copy-treap treap1) (copy-treap treap2))))
;;     (multiple-value-bind (left right) (treap-split 80 (copy-treap treap))
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
;;       (assert (equalp treap1 (treap-delete 41 deleted-treap1))))))

;; (multiple-value-bind (left right) (treap-split 5 (treap-insert 0 (treap-insert 10 (treap-insert 5 nil))))
;;   (assert (= 0 (%treap-key left)))
;;   (assert (null (%treap-left left)))
;;   (assert (null (%treap-right left)))
;;   (assert (or (typep (%treap-left right) 'treap)
;;               (typep (%treap-right right) 'treap))))

