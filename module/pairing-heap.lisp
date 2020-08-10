;;;
;;; Meldable heap (pairing heap)
;;;
;;; Reference:
;;; https://topcoder.g.hatena.ne.jp/spaghetti_source/20120929/1348886107
;;;

;; Note: An empty heap is NIL.
;; TODO: handle the order of heap independently
(defstruct (pheap (:constructor %make-pheap (key))
                  (:conc-name %pheap-)
                  (:copier nil)
                  (:predicate nil))
  key
  (next nil :type (or null pheap))
  (head nil :type (or null pheap)) ; head of children
  )

(declaim (inline pheap-merge))
(defun pheap-merge (node1 node2 order)
  (cond ((null node1) node2)
        ((null node2) node1)
        (t
         ;; ensure NODE1 < NODE2
         (when (funcall order (%pheap-key node2) (%pheap-key node1))
           (rotatef node1 node2))
         (setf (%pheap-next node2) (%pheap-head node1)
               (%pheap-head node1) node2)
         node1)))

;; NOTE: Three implementations are available for MERGE-LIST, each of which has
;; good points and bad points. Currently the third one is adopted.

;; Implementation 1, naive recursion
;; Pros: fastest on SBCL, no consing
;; Cons: there is a risk of stack exhaustion

(declaim (inline %pheap-merge-list1))
(defun %pheap-merge-list1 (node order)
  (labels ((recur (node)
             (when node
               (let* ((a node)
                      (b (%pheap-next node)))
                 (if b
                     (let ((next (%pheap-next b)))
                       (setf (%pheap-next b) nil)
                       (let ((a+b (pheap-merge a b order)))
                         (pheap-merge a+b (recur next) order)))
                     a)))))
    (recur node)))

;; Implementation 2, manual stack by list
;; Pros: stack safe
;; Cons: most consing, 15% slower

(declaim (inline %pheap-merge-list2))
(defun %pheap-merge-list2 (node order)
  (let (stack)
    (loop
      (unless node (return))
      (let ((a node)
            b)
        (setf node (%pheap-next node)
              (%pheap-next a) nil)
        (when node
          (setf b node
                node (%pheap-next node)
                (%pheap-next b) nil))
        (push (pheap-merge a b order) stack)))
    (dolist (part stack)
      (setf node (pheap-merge part node order)))
    node))

;; Implementation 3, manual stack by PHEAP
;; Pros: stack safe, no consing
;; Cons: a bit trickey, 5% slower

(declaim (inline %pheap-merge-list3))
(defun %pheap-merge-list3 (node order)
  (let ((stack (load-time-value (copy-structure (sb-mop:class-prototype (find-class 'pheap))))))
    (setf (%pheap-next stack) nil)
    (loop
      (unless node (return))
      (let ((a node)
            b)
        (setf node (%pheap-next node)
              (%pheap-next a) nil)
        (when node
          (setf b node
                node (%pheap-next node)
                (%pheap-next b) nil))
        (setf a (pheap-merge a b order)
              (%pheap-next a) (%pheap-next stack)
              (%pheap-next stack) a)))
    (loop
      (unless (%pheap-next stack) (return))
      (let ((next (%pheap-next stack)))
        (setf (%pheap-next stack)
              (%pheap-next (%pheap-next stack)))
        (setf node (pheap-merge next node order))))
    node))

(declaim (inline pheep-peek))
(defun pheap-peek (node)
  (%pheap-key node))

;; Here we adopt clojure-like terms CONJ/DISJ as these are not operations to
;; make use of side effects, unlike PUSH/POP.
(declaim (inline pheap-conj))
(defun pheap-conj (node key order)
  (pheap-merge node (%make-pheap key) order))

(declaim (inline pheap-disj))
(defun pheap-disj (node order)
  (declare (pheap node))
  (%pheap-merge-list3 (%pheap-head node) order))

(defmacro pheap-push (key node order)
  `(setf ,node (pheap-conj ,node ,key ,order)))

(defmacro pheap-pop (node order)
  `(prog1 (pheap-peek ,node)
     (setf ,node (pheap-disj ,node ,order))))
