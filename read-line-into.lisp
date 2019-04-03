(declaim (inline read-line-into))
(defun read-line-into (buffer-string &key (in *standard-input*) (terminate-char #\Space))
  (declare (simple-base-string buffer-string))
  (loop for c of-type base-char =
           #-swank (code-char (read-byte in nil #.(char-code #\Newline)))
           #+swank (read-char in nil #\Newline)
        for idx from 0
        until (char= c #\Newline)
        do (setf (schar buffer-string idx) c)
        finally (when (< idx (length buffer-string))
                  (setf (schar buffer-string idx) terminate-char))
                (return (values buffer-string idx))))
