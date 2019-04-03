;; Based on alexandria
(defun map-combinations (function sequence &key (start 0) end length (copy t))
  (declare (function function))
  (let* ((end (or end (length sequence)))
         (size (- end start))
         (length (or length size))
         (combination (subseq sequence 0 length)))
    (declare ((integer 0 #.most-positive-fixnum) start end size length))
    (if (= length size)
        (funcall function combination)
        (flet ((call ()
                 (funcall function (if copy
                                       (copy-seq combination)
                                       combination))))
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

;; I specialized this function only in VECTOR as it requires random access.
(defun map-permutations (function vector &key (start 0) end length (copy t))
  (declare (vector vector)
           (function function))
  (let* ((end (or end (length vector)))
         (size (- end start))
         (length (or length size)))
    (declare ((integer 0 #.most-positive-fixnum) start end size length))
    (labels ((permute (seq n)
               (declare (vector seq)
                        ((integer 0 #.most-positive-fixnum) n))
               (let ((n-1 (- n 1)))
                 (if (zerop n-1)
                     (funcall function (if copy
                                           (copy-seq seq)
                                           seq))
                     (loop for i from 0 upto n-1
                           do (permute seq n-1)
                              (if (evenp n-1)
                                  (rotatef (aref seq 0) (aref seq n-1))
                                  (rotatef (aref seq i) (aref seq n-1)))))))
             (permute-vector (seq)
               (permute seq length)))
      (if (= length size)
          (permute-vector (subseq vector start end))
          (let ((permutation (subseq vector 0 length)))
            (flet ((permute-combination (combination)
                     (declare (vector combination))
                     (permute-vector (replace permutation combination))))
              (declare (dynamic-extent #'permute-combination))
              (map-combinations #'permute-combination vector
                                :start start
                                :end end
                                :length length
                                :copy nil)))))))
