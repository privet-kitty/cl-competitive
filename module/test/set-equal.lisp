(defun set-equal (seq1 seq2 &key (test #'eql))
  (let ((table1 (make-hash-table :test test))
        (table2 (make-hash-table :test test)))
    (loop for x being each element of seq1
          do (setf (gethash x table1) t))
    (loop for x being each element of seq2
          do (setf (gethash x table2) t))
    (and (loop for x being each element of seq1
               always (gethash x table2))
         (loop for x being each element of seq2
               always (gethash x table1)))))
