;;;
;;; Deque implementation with two stacks
;;;

(defstruct (dsdeque (:constructor %make-dsdeque (stack1 stack2))
                    (:conc-name %dsdeque-)
                    (:copier nil))
  (stack1 nil :type list)
  (stack2 nil :type list))

(define-condition dsdeque-empty-error (error)
  ((dsdeque :initarg :deque :reader dsdeque-empty-error-dsdeque))
  (:report
   (lambda (condition stream)
     (format stream "Attempted to pop empty deque ~W" (dsdeque-empty-error-dsdeque condition)))))

(defun make-dsdeque (&optional list)
  (%make-dsdeque list nil))

(declaim (inline dsdeque-push-front))
(defun dsdeque-push-front (obj dsdeque)
  (push obj (%dsdeque-stack1 dsdeque))
  dsdeque)

(declaim (inline dsdeque-push-back))
(defun dsdeque-push-back (obj dsdeque)
  (push obj (%dsdeque-stack2 dsdeque))
  dsdeque)

(defun %dsdeque-balance! (stack)
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

(declaim (inline dsdeque-pop-front))
(defun dsdeque-pop-front (dsdeque)
  (symbol-macrolet ((stack1 (%dsdeque-stack1 dsdeque))
                    (stack2 (%dsdeque-stack2 dsdeque)))
    (unless stack1
      (if (cdr stack2)
          (setf (values stack2 stack1)
                (%dsdeque-balance! stack2))
          (if stack2
              (return-from dsdeque-pop-front (pop stack2))
              (error 'dsdeque-empty-error :deque dsdeque))))
    (pop stack1)))

(declaim (inline dsdeque-pop-back))
(defun dsdeque-pop-back (dsdeque)
  (symbol-macrolet ((stack1 (%dsdeque-stack1 dsdeque))
                    (stack2 (%dsdeque-stack2 dsdeque)))
    (unless stack2
      (if (cdr stack1)
          (setf (values stack1 stack2)
                (%dsdeque-balance! stack1))
          (if stack1
              (return-from dsdeque-pop-back (pop stack1))
              (error 'dsdeque-empty-error :deque dsdeque))))
    (pop stack2)))
