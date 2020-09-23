;;;
;;; Deque implementation with two stacks
;;; (All the basic operations are amortized O(1))
;;;

(defpackage :cp/double-stack-deque
  (:use :cl)
  (:export #:deque-empty-error #:deque-empty-error-deque #:deque #:make-deque
           #:deque-push-back #:deque-push-front #:deque-pop-front #:deque-pop-back
           #:deque-peek-front #:deque-peek-back #:deque-empty-p))
(in-package :cp/double-stack-deque)

(defstruct (deque (:constructor make-deque
                      (&optional list
                       &aux (stack1 list)))
                  (:conc-name %deque-)
                  (:copier nil)
                  (:predicate nil))
  (stack1 nil :type list)
  (stack2 nil :type list))

(define-condition deque-empty-error (error)
  ((deque :initarg :deque :reader deque-empty-error-deque))
  (:report
   (lambda (condition stream)
     (format stream "Attempted to pop empty deque ~W" (deque-empty-error-deque condition)))))

(declaim (inline deque-push-front))
(defun deque-push-front (obj deque)
  (push obj (%deque-stack1 deque))
  deque)

(declaim (inline deque-push-back))
(defun deque-push-back (obj deque)
  (push obj (%deque-stack2 deque))
  deque)

(defun %deque-balance! (stack)
  (declare (optimize (speed 3))
           (list stack))
  (let* ((n (length stack))
         (n/2 (ceiling n 2)))
    (assert (>= n 2))
    (labels ((recur (list pos)
               (declare ((integer 0 #.most-positive-fixnum) pos))
               (if (= (+ pos 1) n/2)
                   (multiple-value-prog1 (values stack (nreverse (cdr list)))
                     (setf (cdr list) nil))
                   (recur (cdr list) (+ pos 1)))))
      (recur stack 0))))

(declaim (inline deque-pop-front))
(defun deque-pop-front (deque)
  (symbol-macrolet ((stack1 (%deque-stack1 deque))
                    (stack2 (%deque-stack2 deque)))
    (unless stack1
      (if (cdr stack2)
          (setf (values stack2 stack1)
                (%deque-balance! stack2))
          (if stack2
              (return-from deque-pop-front (pop stack2))
              (error 'deque-empty-error :deque deque))))
    (pop stack1)))

(declaim (inline deque-pop-back))
(defun deque-pop-back (deque)
  (symbol-macrolet ((stack1 (%deque-stack1 deque))
                    (stack2 (%deque-stack2 deque)))
    (unless stack2
      (if (cdr stack1)
          (setf (values stack1 stack2)
                (%deque-balance! stack1))
          (if stack1
              (return-from deque-pop-back (pop stack1))
              (error 'deque-empty-error :deque deque))))
    (pop stack2)))

(declaim (inline deque-peek-front))
(defun deque-peek-front (deque)
  (symbol-macrolet ((stack1 (%deque-stack1 deque))
                    (stack2 (%deque-stack2 deque)))
    (unless stack1
      (if (cdr stack2)
          (setf (values stack2 stack1)
                (%deque-balance! stack2))
          (if stack2
              (return-from deque-peek-front (car stack2))
              (error 'deque-empty-error :deque deque))))
    (car stack1)))

(declaim (inline deque-peek-back))
(defun deque-peek-back (deque)
  (symbol-macrolet ((stack1 (%deque-stack1 deque))
                    (stack2 (%deque-stack2 deque)))
    (unless stack2
      (if (cdr stack1)
          (setf (values stack1 stack2)
                (%deque-balance! stack1))
          (if stack1
              (return-from deque-peek-back (car stack1))
              (error 'deque-empty-error :deque deque))))
    (car stack2)))

(declaim (inline deque-empty-p))
(defun deque-empty-p (deque)
  (not (or (%deque-stack1 deque)
           (%deque-stack2 deque))))
