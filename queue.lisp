(defstruct (queue (:constructor make-queue
                    (&optional list &aux (tail (last list)))))
  (list nil :type list)
  (tail nil :type (or null (cons t null))))

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
  "Pops OBJ from the front of QUEUE."
  (pop (queue-list queue)))

(declaim (inline queue-empty-p))
(defun queue-empty-p (queue)
  (null (queue-list queue)))

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
