(declaim (inline shuffle!))
(defun shuffle! (vector)
  "Destructively shuffles VECTOR by Fisher-Yates algorithm."
  (declare (vector vector))
  (loop for i from (- (length vector) 1) above 0
        for j = (random (+ i 1))
        do (rotatef (aref vector i) (aref vector j)))
  vector)
