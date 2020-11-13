(defpackage :cp/interactive
  (:use :cl)
  (:export #:interact)
  (:documentation "Provides tester for interactive problem."))
(in-package :cp/interactive)

;; I use a simple blocking queue here, because sb-queue doesn't provide
;; `push-front' function, which is required to implement UNREAD-CHAR.
(defstruct (queue (:constructor make-queue ())
                  (:copier nil)
                  (:predicate nil))
  (lock (sb-thread:make-mutex :name "lock for queue"))
  (list nil :type list)
  (tail nil :type list))

(declaim (inline enqueue))
(defun enqueue (obj queue)
  (symbol-macrolet ((list (queue-list queue))
                    (tail (queue-tail queue)))
    (sb-thread:with-mutex ((queue-lock queue))
      (if list
          (setf (cdr tail) (list obj)
                tail (cdr tail))
          (setf tail (list obj)
                list tail))))
  queue)

(declaim (inline dequeue))
(defun dequeue (queue)
  (sb-thread:with-mutex ((queue-lock queue))
    (pop (queue-list queue))))

(declaim (inline queue-empty-p))
(defun queue-empty-p (queue)
  (sb-thread:with-mutex ((queue-lock queue))
    (null (queue-list queue))))

(declaim (inline queue-peek))
(defun queue-peek (queue)
  (sb-thread:with-mutex ((queue-lock queue))
    (car (queue-list queue))))

(declaim (inline enqueue-front))
(defun enqueue-front (obj queue)
  (symbol-macrolet ((list (queue-list queue))
                    (tail (queue-tail queue)))
    (sb-thread:with-mutex ((queue-lock queue))
      (if list
          (push obj list)
          (setf tail (list obj)
                list tail)))
    queue))

;; NOTE: OCTET-PIPE always takes TIMEOUT seconds until it returns EOF. Given the
;; purpose, it is not a major drawback, though possibly we can do better.
(defclass octet-pipe (sb-gray:fundamental-character-output-stream
                      sb-gray:fundamental-character-input-stream
                      sb-gray:fundamental-binary-output-stream
                      sb-gray:fundamental-binary-input-stream)
  ((queue :initform (make-queue) :initarg :queue :reader octet-pipe-queue)
   (timeout :initform nil :initarg :timeout :reader octet-pipe-timeout)))

(defmethod sb-gray:stream-read-byte ((stream octet-pipe))
  (with-slots (queue timeout) stream
    (or (sb-ext:wait-for (dequeue queue) :timeout timeout)
        :eof)))

(defmethod sb-gray:stream-read-char ((stream octet-pipe))
  (let ((byte (sb-gray:stream-read-byte stream)))
    (if (eql byte :eof) :eof (code-char byte))))

(defmethod sb-gray:stream-write-byte ((stream octet-pipe) integer)
  (check-type integer (unsigned-byte 8))
  (enqueue integer (octet-pipe-queue stream))
  integer)

(defmethod sb-gray:stream-write-char ((stream octet-pipe) character)
  (sb-gray:stream-write-byte stream (char-code character)))


(defmethod sb-gray:stream-peek-char ((stream octet-pipe))
  (with-slots (queue timeout) stream
    (let ((byte (sb-ext:wait-for (queue-peek queue) :timeout timeout)))
      (if (eql byte :eof) :eof (code-char byte)))))

(defmethod sb-gray:stream-unread-char ((stream octet-pipe) character)
  (let ((byte (char-code character)))
    (check-type byte (unsigned-byte 8))
    (enqueue-front byte (octet-pipe-queue stream))
    nil))

(defun interact (solver grader &optional (timeout 1.0))
  "Makes two threads for SOLVER and GRADER, and connect each other's input and
output with pipe."
  (let ((solver-out-grader-in (make-instance 'octet-pipe :timeout timeout))
        (grader-out-solver-in (make-instance 'octet-pipe :timeout timeout)))
    (sb-thread:make-thread
     (lambda (in out)
       (let ((*standard-input* in)
             (*standard-output* out))
         (funcall solver)))
     :arguments (list grader-out-solver-in solver-out-grader-in))
    (sb-thread:make-thread
     (lambda (in out)
       (let ((*standard-input* in)
             (*standard-output* out))
         (funcall grader)))
     :arguments (list solver-out-grader-in grader-out-solver-in))))
