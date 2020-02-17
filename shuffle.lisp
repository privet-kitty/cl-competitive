(declaim (inline shuffle!))
(defun shuffle! (vector &optional (start 0) end)
  "Destructively shuffles VECTOR by Fisher-Yates algorithm."
  (declare (vector vector)
           ((mod #.array-total-size-limit) start)
           ((or null (mod #.array-total-size-limit)) end))
  (loop for i from (- (or end (length vector)) 1) above start
        for j = (+ start (random (- (+ i 1) start)))
        do (rotatef (aref vector i) (aref vector j)))
  vector)
