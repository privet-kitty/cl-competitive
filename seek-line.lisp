;; Ensures that the next character of the stream is neither LF nor CR.
(defun seek-line (&optional (stream *standard-input*))
  (declare (optimize (speed 3)))
  (loop while (let ((c (peek-char nil stream nil #\eot)))
		(if (char= c #\eot)
		    (return-from seek-line)
		    (or (char= c #\lf)
			(char= c #\cr))))
        do (read-char stream)))
