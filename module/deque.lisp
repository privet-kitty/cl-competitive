;;;
;;; double-ended queue (ring buffer)
;;;

(defpackage :cp/deque
  (:use :cl)
  (:export #:deque-empty-error #:deque-full-error #:deque-invalid-index-error #:define-deque))
(in-package :cp/deque)

(declaim (inline %power-of-two-ceiling))
(defun %power-of-two-ceiling (x)
  (ash 1 (integer-length (- x 1))))

(deftype index () '(integer 0 #.array-total-size-limit))

(define-condition deque-empty-error (error)
  ((queue :initarg :queue :reader deque-empty-error-queue))
  (:report
   (lambda (condition stream)
     (format stream "Attempted to pop empty deque ~W" (deque-empty-error-queue condition)))))

(define-condition deque-full-error (error)
  ((queue :initarg :queue :reader deque-full-error-queue)
   (item :initarg :item :reader deque-full-error-item))
  (:report
   (lambda (condition stream)
     (format stream "Attempted to push item ~W to full deque ~W"
             (deque-full-error-item condition)
             (deque-full-error-queue condition)))))

(define-condition deque-invalid-index-error (error)
  ((queue :initarg :queue :reader deque-invalid-index-error-queue)
   (index :initarg :index :reader deque-invalid-index-error-index))
  (:report
   (lambda (condition stream)
     (format stream "Invalid index ~W for ~W."
             (deque-invalid-index-error-index condition)
             (deque-invalid-index-error-queue condition)))))

;; TODO: detailed documentation
;; TODO: add setter; adjust if deque is full
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
        (peek-front (intern (format nil "~A-PEEK-FRONT" name)))
        (peek-back (intern (format nil "~A-PEEK-BACK" name)))
        (empty-p (intern (format nil "~A-EMPTY-P" name)))
        (constructor (intern (format nil "MAKE-~A" name)))
        (reinitializer (intern (format nil "~A-REINITIALIZE" name)))
        (reffer (intern (format nil "~A-REF" name)))
        (data-getter (intern (format nil "~A-DATA" name)))
        (front-getter (intern (format nil "~A-FRONT" name)))
        (count-getter (intern (format nil "~A-COUNT" name)))
        (adjuster (intern (format nil "%~A-ADJUST" name))))
    `(progn
       (defstruct (,name (:constructor ,constructor
                             (size
                              &aux
                              (data (make-array (max 2 (%power-of-two-ceiling size))
                                                :element-type
                                                ',element-type))))
                         (:copier nil)
                         (:predicate nil))
         (data nil :type (simple-array ,element-type (*)))
         (front 0 :type index)
         (count 0 :type index))
       (declaim (ftype (function (index) (values ,name &optional))
                       ,constructor))

       (defun ,adjuster (,name)
         (declare (optimize (speed 3)))
         (symbol-macrolet ((front (,front-getter ,name))
                           (count (,count-getter ,name))
                           (data (,data-getter ,name)))
           (let ((length (length data)))
             (when (= count length)
               (let ((new-data (make-array (* 2 length) :element-type ',element-type)))
                 (replace new-data data :start2 front)
                 (when (< length (+ front count))
                   (replace new-data data :start1 (- length front) :end2 front))
                 (setq data new-data
                       front 0))))))

       (declaim (inline ,push-front))
       (defun ,push-front (obj ,name)
         (symbol-macrolet ((front (,front-getter ,name))
                           (count (,count-getter ,name)))
           (,adjuster ,name)
           (let* ((data (,data-getter ,name))
                  (length (length data))
                  (mask (- length 1))
                  (next-front (logand mask (+ front mask))))
             (declare (index next-front))
             (setf (aref data next-front) obj)
             (setq front next-front)
             (incf count)
             ,name)))

       (declaim (inline ,pop-front))
       (defun ,pop-front (,name)
         (symbol-macrolet ((front (,front-getter ,name))
                           (count (,count-getter ,name)))
           (when (zerop count)
             (error 'deque-empty-error :queue ,name))
           (let* ((data (,data-getter ,name))
                  (length (length data))
                  (mask (- length 1))
                  (next (logand mask (+ front 1))))
             (declare (index next))
             (prog1 (aref data front)
               (setq front next)
               (decf count)))))

       (declaim (inline ,peek-front))
       (defun ,peek-front (,name)
         (symbol-macrolet ((front (,front-getter ,name))
                           (count (,count-getter ,name)))
           (when (zerop count)
             (error 'deque-empty-error :queue ,name))
           (let* ((data (,data-getter ,name)))
             (aref data front))))

       (declaim (inline ,push-back))
       (defun ,push-back (obj ,name)
         (symbol-macrolet ((front (,front-getter ,name))
                           (count (,count-getter ,name)))
           (,adjuster ,name)
           (let* ((data (,data-getter ,name))
                  (length (length data))
                  (mask (- length 1))
                  (pos (logand mask (+ front count))))
             (declare (index pos))
             (setf (aref data pos) obj)
             (incf count)
             ,name)))

       (declaim (inline ,pop-back))
       (defun ,pop-back (,name)
         (symbol-macrolet ((front (,front-getter ,name))
                           (count (,count-getter ,name)))
           (when (zerop count)
             (error 'deque-empty-error :queue ,name))
           (let* ((data (,data-getter ,name))
                  (length (length data))
                  (mask (- length 1))
                  (pos (logand mask (+ front count mask))))
             (declare (index pos))
             (decf count)
             (aref data pos))))

       (declaim (inline ,peek-back))
       (defun ,peek-back (,name)
         (symbol-macrolet ((front (,front-getter ,name))
                           (count (,count-getter ,name)))
           (when (zerop count)
             (error 'deque-empty-error :queue ,name))
           (let* ((data (,data-getter ,name))
                  (length (length data))
                  (mask (- length 1))
                  (pos (logand mask (+ front count mask))))
             (declare (index pos))
             (aref data pos))))

       (declaim (inline ,empty-p))
       (defun ,empty-p (,name)
         (zerop (,count-getter ,name)))

       (declaim (inline ,reinitializer))
       (defun ,reinitializer (,name)
         (setf (,count-getter ,name) 0))

       (declaim (inline ,reffer))
       (defun ,reffer (,name index)
         "Returns the INDEX-th element from the top. (0-based)"
         (declare (index index))
         (symbol-macrolet ((front (,front-getter ,name))
                           (count (,count-getter ,name)))
           (when (>= index count)
             (error 'deque-invalid-index-error :index index :queue ,name))
           (let* ((data (,data-getter ,name))
                  (length (length data))
                  (mask (- length 1))
                  (pos (logand mask (+ front index))))
             (declare (index pos))
             (aref data pos)))))))

#+(or)
(define-deque deque :element-type fixnum)
