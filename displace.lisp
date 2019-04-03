(defun displace (vector &optional (start 0) end)
  "displaced subseq"
  (let ((end (or end (length vector))))
    (make-array (- end start)
                :element-type (array-element-type vector)
                :displaced-to vector
                :displaced-index-offset start)))
