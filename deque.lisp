;; naivest implementation of deque
;; It doesn't serve a serious purpose.

(defstruct (deque (:constructor make-deque (one-direction-size
                                            &key (element-type t)
                                            &aux (front-pos (- one-direction-size 1))
                                                 (back-pos one-direction-size)
                                                 (data (make-array (list (* 2 one-direction-size)) :element-type element-type)))))
  (data #() :type (simple-array * (*)))
  (front-pos 0 :type (integer 0 #.array-total-size-limit))
  (back-pos 0 :type (integer 0 #.array-total-size-limit)))

(defun deq-push-back (obj deq)
  (setf (aref (deque-data deq) (deque-back-pos deq)) obj)
  (incf (deque-back-pos deq)))

(defun deq-push-front (obj deq)
  (setf (aref (deque-data deq) (deque-front-pos deq)) obj)
  (decf (deque-front-pos deq)))

(defun deq-pop-back (deq &optional (error t) null-value)
  (symbol-macrolet ((data (deque-data deq))
                    (front-pos (deque-front-pos deq))
                    (back-pos (deque-back-pos deq)))
    (if (<= (- back-pos front-pos) 1)
        (if error
            (error "No element in deque")
            null-value)
        (aref data (decf back-pos)))))

(defun deq-pop-front (deq &optional (error t) null-value)
  (symbol-macrolet ((data (deque-data deq))
                    (front-pos (deque-front-pos deq))
                    (back-pos (deque-back-pos deq)))
    (if (<= (- back-pos front-pos) 1)
        (if error
            (error "No element in deque")
            null-value)
        (aref data (incf front-pos)))))
