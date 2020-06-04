;; DEPRECATED
(defmacro with-buffered-stdout (&body body)
  "Buffers all outputs to *STANDARD-OUTPUT* in BODY and flushes them to
*STANDARD-OUTPUT* after BODY has been done (without error). Note that only
BASE-CHAR is allowed."
  (let ((out (gensym)))
    `(let ((,out (make-string-output-stream :element-type 'base-char)))
       (let ((*standard-output* ,out))
         ,@body)
       (write-string (get-output-stream-string ,out)))))
