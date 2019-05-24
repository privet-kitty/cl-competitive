;; Based on alexandria
;; TODO: Implement Chase's algorithm
(declaim (inline map-combinations))
(defun map-combinations (function sequence &key (start 0) end length)
  (declare (function function))
  (let* ((end (or end (length sequence)))
         (size (- end start))
         (length (or length size))
         (combination (subseq sequence 0 length)))
    (declare ((integer 0 #.most-positive-fixnum) start end size length))
    (flet ((call () (nth-value 0 (funcall function combination))))
      (declare (inline call))
      (if (= length size)
          (call)
          (etypecase sequence
            (list
             (labels ((combine-list (c-tail o-tail)
                        (if (not c-tail)
                            (call)
                            (do ((tail o-tail (cdr tail)))
                                ((not tail))
                              (setf (car c-tail) (car tail))
                              (combine-list (cdr c-tail) (cdr tail))))))
               (combine-list combination (nthcdr start sequence))))
            (vector
             (labels ((combine (count start)
                        (if (zerop count)
                            (call)
                            (loop for i from start below end
                                  do (let ((j (- count 1)))
                                       (setf (aref combination j) (aref sequence i))
                                       (combine j (+ i 1)))))))
               (combine length start)))
            (sequence
             (labels ((combine (count start)
                        (if (zerop count)
                            (call)
                            (loop for i from start below end
                                  do (let ((j (- count 1)))
                                       (setf (elt combination j) (elt sequence i))
                                       (combine j (+ i 1)))))))
               (combine length start)))))))
  sequence)

(defmacro do-combinations ((var sequence &key (start 0) end length) &body body)
  `(map-combinations (lambda (,var) ,@body)
                     ,sequence
                     :start ,start
                     :end ,end
                     :length ,length))


