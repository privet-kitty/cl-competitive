(declaim (inline read-schar))
(defun read-schar (&optional (stream *standard-input*))
  (declare #-swank (sb-kernel:ansi-stream stream)
           (inline read-byte))
  #+swank (read-char stream nil #\Newline) ; on SLIME
  #-swank (code-char (read-byte stream nil #.(char-code #\Newline))))
