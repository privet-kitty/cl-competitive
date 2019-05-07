(declaim (inline map-binsorted))
(defun map-binsorted (function sequence range-max &key from-end)
  "Calls FUNCTION with each ascending (descending) non-negative integer in
SEQUENCE if FROM-END is false (true). Any integers in SEQUENCE must not exceed
RANGE-MAX."
  (declare (function function)
           ((mod #.array-total-size-limit) range-max))
  (let ((counts (make-array (1+ range-max) :element-type 'fixnum :initial-element 0)))
    (declare (dynamic-extent counts))
    (let ((existing-min most-positive-fixnum)
          (existing-max 0))
      (etypecase sequence
        (vector (loop for e across sequence
                      do (incf (aref counts e))
                         (when (< e existing-min) (setf existing-min e))
                         (when (< existing-max e) (setf existing-max e))))
        (list (dolist (e sequence)
                (incf (aref counts e))
                (when (< e existing-min) (setf existing-min e))
                (when (< existing-max e) (setf existing-max e)))))
      (if from-end
          (loop for v from existing-max downto existing-min
                do (loop repeat (aref counts v)
                         do (funcall function v)))
          (loop for v from existing-min to existing-max
                do (loop repeat (aref counts v)
                         do (funcall function v)))))))

(defmacro do-binsorted ((var sequence range-max &key from-end finally) &body body)
  `(block nil
     (map-binsorted (lambda (,var) ,@body) ,sequence ,range-max :from-end ,from-end)
     ,finally))
