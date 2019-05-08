;; Based on alexandria
(defun map-combinations (function sequence &key (start 0) end length)
  (declare (function function))
  (let* ((end (or end (length sequence)))
         (size (- end start))
         (length (or length size))
         (combination (subseq sequence 0 length)))
    (declare ((integer 0 #.most-positive-fixnum) start end size length))
    (if (= length size)
        (funcall function combination)
        (flet ((call ()
                 (funcall function combination)))
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

