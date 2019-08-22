;;;
;;; double-ended queue (ring buffer)
;;;

(define-condition deque-empty-error (simple-error)
  ((queue :initarg :queue :reader deque-empty-error-queue))
  (:report
   (lambda (condition stream)
     (format stream "Attempted to pop empty deque ~W" (deque-empty-error-queue condition)))))

(define-condition deque-full-error (simple-error)
  ((queue :initarg :queue :reader deque-full-error-queue)
   (item :initarg :item :reader deque-full-error-item))
  (:report
   (lambda (condition stream)
     (format stream "Attempted to push item ~W to full deque ~W"
             (deque-full-error-item condition)
             (deque-full-error-queue condition)))))

(define-condition deque-invalid-index-error (simple-error)
  ((queue :initarg :queue :reader deque-invalid-index-error-queue)
   (index :initarg :index :reader deque-invalid-index-error-index))
  (:report
   (lambda (condition stream)
     (format stream "Invalid index ~W for ~W."
             (deque-invalid-index-error-index condition)
             (deque-invalid-index-error-queue condition)))))

;; TODO: detailed documentation
(defmacro define-deque (name &key (element-type 'fixnum))
  "Defines deque for given ELEMENT-TYPE.

constructor: MAKE-<NAME>.
basic operations: <NAME>-PUSH-FRONT, <NAME>-PUSH-BACK, <NAME>-POP-FRONT,
<NAME>-POP-BACK.
accessor: <NAME>-REF.
utilities: <NAME>-EMPTY-P, <NAME>-REINITIALIZE.
"
  (let ((push-front (intern (format nil "~A-PUSH-FRONT" name)))
        (push-back (intern (format nil "~A-PUSH-BACK" name)))
        (pop-front (intern (format nil "~A-POP-FRONT" name)))
        (pop-back (intern (format nil "~A-POP-BACK" name)))
        (empty-p (intern (format nil "~A-EMPTY-P" name)))
        (constructor (intern (format nil "MAKE-~A" name)))
        (reinitializer (intern (format nil "~A-REINITIALIZE" name)))
        (reffer (intern (format nil "~A-REF" name)))
        (data-getter (intern (format nil "~A-DATA" name)))
        (front-getter (intern (format nil "~A-FRONT" name)))
        (count-getter (intern (format nil "~A-COUNT" name))))
    `(progn
       (defstruct (,name (:constructor ,constructor
                             (size &aux
                                   (data (progn
                                           (check-type size (integer 0))
                                           (make-array (+ 1 size) :element-type ',element-type)))))
                         (:copier nil)
                         (:predicate nil))
         (data nil :type (simple-array ,element-type (*)))
         (front 0 :type (integer 0 #.most-positive-fixnum))
         (count 0 :type (integer 0 #.most-positive-fixnum)))

       (declaim (inline ,push-front))
       (defun ,push-front (obj ,name)
         (symbol-macrolet ((front (,front-getter ,name))
                           (count (,count-getter ,name)))
           (let* ((data (,data-getter ,name))
                  (length (length data))
                  (next (- front 1)))
             (declare ((integer -1 #.most-positive-fixnum) next))
             (when (< next 0) (incf next length))
             (when (= (+ 1 count) length)
               (error 'deque-full-error :item obj :queue ,name))
             (setf (aref data front) obj)
             (setq front next)
             (incf count)
             ,name)))

       (declaim (inline ,pop-front))
       (defun ,pop-front (,name)
         (symbol-macrolet ((front (,front-getter ,name))
                           (count (,count-getter ,name)))
           (let* ((data (,data-getter ,name))
                  (length (length data))
                  (next (+ front 1)))
             (declare ((integer 0 #.most-positive-fixnum) next))
             (when (= next length) (decf next length))
             (when (zerop count)
               (error 'deque-empty-error :queue ,name))
             (setq front next)
             (decf count)
             (aref data next))))

       (declaim (inline ,push-back))
       (defun ,push-back (obj ,name)
         (symbol-macrolet ((front (,front-getter ,name))
                           (count (,count-getter ,name)))
           (let* ((data (,data-getter ,name))
                  (length (length data))
                  (pos (+ front count 1)))
             (declare ((integer 0 #.most-positive-fixnum) pos))
             (when (>= pos length) (decf pos length))
             (when (= (+ 1 count) length)
               (error 'deque-full-error :item obj :queue ,name))
             (setf (aref data pos) obj)
             (incf count)
             ,name)))

       (declaim (inline ,pop-back))
       (defun ,pop-back (,name)
         (symbol-macrolet ((front (,front-getter ,name))
                           (count (,count-getter ,name)))
           (let* ((data (,data-getter ,name))
                  (length (length data))
                  (pos (+ front count)))
             (declare ((integer 0 #.most-positive-fixnum) pos))
             (when (>= pos length) (decf pos length))
             (when (zerop count)
               (error 'deque-empty-error :queue ,name))
             (prog1 (aref data pos)
               (decf count)))))

       (declaim (inline ,empty-p))
       (defun ,empty-p (,name)
         (zerop (,count-getter ,name)))

       (declaim (inline ,reinitializer))
       (defun ,reinitializer (,name)
         (setf (,count-getter ,name) 0))

       (declaim (inline ,reffer))
       (defun ,reffer (,name index)
         "Returns the INDEX-th element from the top. (0-based)"
         (declare ((integer 0 #.most-positive-fixnum) index))
         (let* ((data (,data-getter ,name))
                (count (,count-getter ,name))
                (front (,front-getter ,name))
                (length (length data))
                (pos (+ front index 1)))
           (declare ((integer 0 #.most-positive-fixnum) pos))
           (when (>= pos length) (decf pos length))
           (when (>= index count)
             (error 'deque-invalid-index-error :index index :queue ,name))
           (aref data pos))))))

(define-deque deque :element-type fixnum)
