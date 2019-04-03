(setf *print-circle* t)

;; Treap accessible by index (O(log(n)))
(defstruct (treap (:constructor make-treap (key priority &key left right (count 1)))
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

(defun treap-find (key treap &key (test #'<))
  "Finds the sub-treap of TREAP whose key satisfies (and (not (funcall test
key (%treap-key sub-treap))) (not (funcall test (%treap-key sub-treap) key)))
and returns KEY. Returns NIL if KEY is not contained."
  (declare (optimize (speed 3)) ; TCO
           (function test)
           ((or null treap) treap))
  (cond ((null treap) nil)
        ((funcall test key (%treap-key treap))
         (treap-find key (%treap-left treap) :test test))
        ((funcall test (%treap-key treap) key)
         (treap-find key (%treap-right treap) :test test))
        (t key)))

(defun treap (test &rest keys)
  (loop with res = nil
        for key in keys
        do (setf res (treap-insert key res :test test))
        finally (return res)))

(defun treap-bisect-left (value treap &key (test #'<))
  "Returns the smallest index and the corresponding key that satisfies
TREAP[index] >= VALUE. Returns the size of TREAP and VALUE if TREAP[size-1] <
VALUE."
  (declare #.OPT (function test))
  (labels ((recurse (count treap)
             (declare ((integer 0 #.most-positive-fixnum) count))
             (cond ((null treap) (values nil nil))
                   ((funcall test (%treap-key treap) value)
                    (recurse count (%treap-right treap)))
                   (t (let ((left-count (- count (treap-count (%treap-right treap)) 1)))
                        (multiple-value-bind (idx key)
                            (recurse left-count (%treap-left treap))
                          (if idx
                              (values idx key)
                              (values left-count (%treap-key treap)))))))))
    (declare (ftype (function * (values t t &optional)) recurse))
    (multiple-value-bind (idx key)
        (recurse (treap-count treap) treap)
      (if idx
          (values idx key)
          (values (treap-count treap) value)))))

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
           (update-count treap)
           (values treap right)))
        (t
         (multiple-value-bind (left right)
             (treap-split key (%treap-left treap) :test test)
           (setf (%treap-left treap) right)
           (update-count treap)
           (values left treap)))))

(declaim (inline treap-insert))
(defun treap-insert (key treap &key (test #'<))
  "Destructively inserts KEY into TREAP and returns the result treap. You cannot
rely on the side effect. Use the returned value.

The behavior is undefined when duplicated keys are inserted."
  (declare ((or null treap) treap)
           (function test))
  (labels ((recurse (node treap)
             (declare (treap node))
             (cond ((null treap) node)
                   ((> (%treap-priority node) (%treap-priority treap))
                    (setf (values (%treap-left node) (%treap-right node))
                          (treap-split (%treap-key node) treap :test test))
                    (update-count node)
                    node)
                   (t
                    (if (funcall test (%treap-key node) (%treap-key treap))
                        (setf (%treap-left treap)
                              (recurse node (%treap-left treap)))
                        (setf (%treap-right treap)
                              (recurse node (%treap-right treap))))
                    (update-count treap)
                    treap))))
    (recurse (make-treap key (random most-positive-fixnum)) treap)))

(defun treap-merge (left right)
  "Destructively merges two treaps. Assumes that all keys of LEFT are smaller (or larger,
depending on the order) than those of RIGHT."
  (declare ((or null treap) left right))
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

(defun treap-delete (key treap &key (test #'<))
  "Destructively deletes the KEY in TREAP and returns the result treap. You
cannot rely on the side effect. Use the returned value.

Note that this function deletes at most one node even if duplicated keys exist."
  (declare ((or null treap) treap)
           (function test))
  (cond ((null treap) nil)
        ((funcall test key (%treap-key treap))
         (setf (%treap-left treap) (treap-delete key (%treap-left treap) :test test))
         (update-count treap)
         treap)
        ((funcall test (%treap-key treap) key)
         (setf (%treap-right treap) (treap-delete key (%treap-right treap) :test test))
         (update-count treap)
         treap)
        (t
         (treap-merge (%treap-left treap) (%treap-right treap)))))

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
  "Index access"
  (declare (optimize (speed 3)) ; TCO
           ((or null treap) treap)
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
                     (t (%treap-key treap))))))
    (%ref treap index)))

(defun copy-treap (treap)
  "For development. Recursively copies the whole TREAP."
  (declare ((or null treap) treap))
  (if (null treap)
      nil
      (make-treap (%treap-key treap)
                  (%treap-priority treap)
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

