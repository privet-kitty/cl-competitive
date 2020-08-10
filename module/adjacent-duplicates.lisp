(declaim (inline delete-adjacent-duplicates))
(defun delete-adjacent-duplicates (seq &key (test #'eql))
  "Destructively deletes adjacent duplicates of SEQ: e.g. #(1 1 1 2 2 1 3) ->
#(1 2 1 3)"
  (declare (sequence seq)
           (function test))
  (etypecase seq
    (vector
     (if (zerop (length seq))
         seq
         (let ((prev (aref seq 0))
               (end 1))
           (loop for pos from 1 below (length seq)
                 unless (funcall test prev (aref seq pos))
                 do (setf prev (aref seq pos)
                          (aref seq end) (aref seq pos)
                          end (+ 1 end)))
           ;; KLUDGE: Resorting to ADJUST-ARRAY is maybe substandard. 
           (if (array-has-fill-pointer-p seq)
               (adjust-array seq end :fill-pointer end)
               (adjust-array seq end)))))
    (list
     (loop for rest on seq
           unless (and (cdr rest)
                       (funcall test (first rest) (second rest)))
           collect (car rest)))))
