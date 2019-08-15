;; Not included in test script. Better to use ref-able-treap instead.

(defstruct (treap (:constructor %make-treap (key priority &optional left right))
                  (:copier nil)
                  (:conc-name %treap-))
  key
  (priority 0 :type (integer 0 #.most-positive-fixnum))
  (left nil :type (or null treap))
  (right nil :type (or null treap)))

(declaim (inline treap-find))
(defun treap-find (key treap &key (test #'<))
  "Searches the sub-treap of TREAP whose key satisfies (and (not (funcall test
key (%treap-key sub-treap))) (not (funcall test (%treap-key sub-treap) key))) and
returns KEY. Returns NIL if KEY is not contained."
  (declare (function test)
           ((or null treap) treap))
  (labels ((recur (treap)
             (cond ((null treap) nil)
                   ((funcall test key (%treap-key treap))
                    (recur (%treap-left treap)))
                   ((funcall test (%treap-key treap) key)
                    (recur (%treap-right treap)))
                   (t key))))
    (recur treap)))

(declaim (inline treap-split))
(defun treap-split (key treap &key (test #'<))
  "Destructively splits the TREAP with reference to KEY and returns two treaps,
the smaller sub-treap (< KEY) and the larger one (>= KEY)."
  (declare (function test)
           ((or null treap) treap))
  (labels ((recur (treap)
             (cond ((null treap)
                    (values nil nil))
                   ((funcall test (%treap-key treap) key)
                    (multiple-value-bind (left right)
                        (recur (%treap-right treap))
                      (setf (%treap-right treap) left)
                      (values treap right)))
                   (t
                    (multiple-value-bind (left right)
                        (recur (%treap-left treap))
                      (setf (%treap-left treap) right)
                      (values left treap))))))
    (recur treap)))

(declaim (inline treap-insert))
(defun treap-insert (key treap &key (test #'<))
  "Destructively inserts KEY into TREAP and returns the resultant treap. You
cannot rely on the side effect. Use the returned value.

The behavior is undefined when duplicated keys are inserted."
  (declare ((or null treap) treap)
           (function test))
  (labels ((recur (new-node treap)
             (declare (treap new-node))
             (cond ((null treap) new-node)
                   ((> (%treap-priority new-node) (%treap-priority treap))
                    (setf (values (%treap-left new-node) (%treap-right new-node))
                          (treap-split (%treap-key new-node) treap :test test))
                    new-node)
                   (t
                    (if (funcall test (%treap-key new-node) (%treap-key treap))
                        (setf (%treap-left treap)
                              (recur new-node (%treap-left treap)))
                        (setf (%treap-right treap)
                              (recur new-node (%treap-right treap))))
                    treap))))
    (recur (%make-treap key (random most-positive-fixnum)) treap)))

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
  "Destructively concatenates two treaps. Assumes that all keys of LEFT are
smaller (or larger, depending on the order) than those of RIGHT."
  (declare (optimize (speed 3))
           ((or null treap) left right))
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

(declaim (inline treap-delete))
(defun treap-delete (key treap &key (test #'<))
  "Destructively deletes the KEY in TREAP and returns the resultant treap. You
cannot rely on the side effect. Use the returned value."
  (declare ((or null treap) treap)
           (function test))
  (labels ((recur (treap)
             (cond ((null treap) nil)
                   ((funcall test key (%treap-key treap))
                    (setf (%treap-left treap) (recur (%treap-left treap)))
                    treap)
                   ((funcall test (%treap-key treap) key)
                    (setf (%treap-right treap) (recur (%treap-right treap)))
                    treap)
                   (t
                    (treap-merge (%treap-left treap) (%treap-right treap))))))
    (recur treap)))

(declaim (ftype (function * (values (integer 0 #.most-positive-fixnum) &optional)) treap-count))
(defun treap-count (treap)
  "Computes the length of TREAP in O(n)."
  (declare (optimize (speed 3)) ((or null treap) treap))
  (labels ((recur (treap)
             (declare (optimize (safety 0)))
             (if (null treap)
                 0
                 (+ 1 (treap-count (%treap-left treap)) (treap-count (%treap-right treap))))))
    (recur treap)))

;; (defun copy-treap (treap)
;;   "For development. Recursively copies the whole TREAP."
;;   (declare ((or null treap) treap))
;;   (if (null treap)
;;       nil
;;       (%make-treap (%treap-key treap)
;;                   (%treap-priority treap)
;;                   (copy-treap (%treap-left treap))
;;                   (copy-treap (%treap-right treap)))))

;; Test
;; (let ((treap1 (%make-treap 50 15))
;;       (treap2 (%make-treap 100 11)))
;;   (setf (%treap-left treap1) (%make-treap 30 5))
;;   (setf (%treap-left (%treap-left treap1)) (%make-treap 20 2))
;;   (setf (%treap-right (%treap-left treap1)) (%make-treap 40 4))
;;   (setf (%treap-right treap1) (%make-treap 70 10))
;;   (setf (%treap-right treap2) (%make-treap 200 3))
;;   (setf (%treap-left treap2) (%make-treap 99 5))
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

