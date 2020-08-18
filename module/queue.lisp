;;;
;;; Queue with singly linked list
;;;

(defpackage :cp/queue
  (:use :cl)
  (:export #:queue #:make-queue #:enqueue #:dequeue #:queue-empty-p #:queue-peek #:enqueue-front))
(in-package :cp/queue)

(defstruct (queue (:constructor make-queue
                      (&optional list &aux (tail (last list))))
                  (:copier nil)
                  (:predicate nil))
  (list nil :type list)
  (tail nil :type list))

(declaim (inline enqueue))
(defun enqueue (obj queue)
  "Pushes OBJ to the end of QUEUE."
  (symbol-macrolet ((list (queue-list queue))
                    (tail (queue-tail queue)))
    (if (null list)
        (setf tail (list obj)
              list tail)
        (setf (cdr tail) (list obj)
              tail (cdr tail))))
  queue)

(declaim (inline dequeue))
(defun dequeue (queue)
  "Removes and returns the element at the front of QUEUE. Returns NIL if QUEUE
is empty."
  (pop (queue-list queue)))

(declaim (inline queue-empty-p))
(defun queue-empty-p (queue)
  (null (queue-list queue)))

(declaim (inline queue-peek))
(defun queue-peek (queue)
  (car (queue-list queue)))

(declaim (inline enqueue-front))
(defun enqueue-front (obj queue)
  "Pushes OBJ to the front of QUEUE."
  (symbol-macrolet ((list (queue-list queue))
                    (tail (queue-tail queue)))
    (if (null list)
        (setf tail (list obj)
              list tail)
        (push obj list))
    queue))
