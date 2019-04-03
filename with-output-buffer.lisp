(defmacro with-output-buffer (&body body)
  "Buffers all outputs to *STANDARD-OUTPUT* and flushes them to
*STANDARD-OUTPUT* at the end. Note that only BASE-CHAR is allowed."
  (let ((out (gensym)))
    `(let ((,out (make-string-output-stream :element-type 'base-char)))
       (let ((*standard-output* ,out))
         ,@body)
       (write-string (get-output-stream-string ,out)))))
