;;;
;;; Persistent vector (unfinished)
;;;

;; NOTE: not tested

;; TODO:
;; - more sane handling of unbounded index
;; - handy function for initialization (currently `unbounded value' is zero)
;; - iteration; map
;; - abstraction
;; - printer

(defconstant +persistent-vector-log+ 16)

(declaim (inline %make-persistent-vector))
(defstruct (persistent-vector (:constructor %make-persistent-vector ())
                              (:conc-name %pv-))
  (value 0 :type fixnum)
  (children nil :type (or null (simple-vector #.+persistent-vector-log+))))

(defun pv-assoc (pvector index value)
  "Returns a new persistent vector whose value at INDEX is modified to
VALUE. PVECTOR can be null."
  (declare ((or null persistent-vector) pvector)
           ((integer 0 #.most-positive-fixnum) index))
  (labels ((recur (pvector index)
             (declare ((or (integer 0 0) persistent-vector) pvector)
                      ((integer 0 #.most-positive-fixnum) index))
             (let ((res (%make-persistent-vector)))
               (if (eql 0 pvector)
                   (setf (%pv-children res)
                         (make-array +persistent-vector-log+ :initial-element 0))
                   (setf (%pv-children res) (copy-seq (%pv-children pvector))
                         (%pv-value res) (%pv-value pvector)))
               (if (zerop index)
                   (setf (%pv-value res) value)
                   (setf (aref (%pv-children res) (mod index +persistent-vector-log+))
                         (recur (aref (%pv-children res) (mod index +persistent-vector-log+))
                                (floor index +persistent-vector-log+))))
               res)))
    (recur (or pvector 0) index)))

(defun pv-ref (pvector index)
  "Returns a value at INDEX.

NOTE: currently unbounded (or out-of-bound) value is zero."
  (declare ((or null persistent-vector) pvector))
  (labels ((recur (pvector index)
             (declare ((or (integer 0 0) persistent-vector) pvector)
                      ((integer 0 #.most-positive-fixnum) index))
             (cond ((eql 0 pvector) 0)
                   ((zerop index) (%pv-value pvector))
                   (t (recur (aref (%pv-children pvector)
                                   (mod index +persistent-vector-log+))
                             (floor index +persistent-vector-log+))))))
    (recur (or pvector 0) index)))
;;;
;;; Queue with singly linked list
;;;

;; (defstruct (queue (:constructor make-queue
;;                       (&optional list &aux (tail (last list)))))
;;   (list nil :type list)
;;   (tail nil :type list))

;; (declaim (inline enqueue))
;; (defun enqueue (obj queue)
;;   "Pushes OBJ to the end of QUEUE."
;;   (symbol-macrolet ((list (queue-list queue))
;;                     (tail (queue-tail queue)))
;;     (if (null list)
;;         (setf tail (list obj)
;;               list tail)
;;         (setf (cdr tail) (list obj)
;;               tail (cdr tail))))
;;   queue)

;; (declaim (inline dequeue))
;; (defun dequeue (queue)
;;   "Removes and returns the element at the front of QUEUE. Returns NIL if QUEUE
;; is empty."
;;   (pop (queue-list queue)))

;; (declaim (inline queue-empty-p))
;; (defun queue-empty-p (queue)
;;   (null (queue-list queue)))

;; (declaim (inline queue-peek))
;; (defun queue-peek (queue)
;;   (car (queue-list queue)))

;; (declaim (inline enqueue-front))
;; (defun enqueue-front (obj queue)
;;   "Pushes OBJ to the front of QUEUE."
;;   (symbol-macrolet ((list (queue-list queue))
;;                     (tail (queue-tail queue)))
;;     (if (null list)
;;         (setf tail (list obj)
;;               list tail)
;;         (push obj list))
;;     queue))

;; (defmethod print-object ((object persistent-vector) stream)
;;   (print-unreadable-object (object stream :type t)
;;     (let ((res (make-array 1))
;;           (que (make-queue)))
;;       (labels ((%set (index value)
;;                  (when (>= index (length res))
;;                    (setq res (adjust-array res (* 2 index)))))
;;                (recur (pvector depth)
;;                  (setf (aref res (expt ))))))
;;       (loop until (queue-empty-p que)
;;             for pvector = (dequeue que)
;;             when (persistent-vector-p pvector)
;;             do (push (%pv-value pvector) res)
;;                (loop for child across (%pv-children pvector)
;;                      do (enqueue child que))
;;             else
;;             do (loop repeat +persistent-vector-log+
;;                      do (push 0 res)))
;;       (format stream "~{~A~^ ~}" (nreverse res)))))
