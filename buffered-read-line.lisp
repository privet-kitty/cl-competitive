(defmacro buffered-read-line (&optional (buffer-size 30) (in '*standard-input*) (terminate-char #\Space))
  "Note that the returned string will be reused."
  (let ((buffer (gensym))
        (character (gensym))
        (idx (gensym)))
    `(let ((,buffer (load-time-value (make-string ,buffer-size
                                                  :element-type 'base-char))))
       (declare (simple-base-string ,buffer))
       (loop for ,character of-type base-char =
                #-swank (code-char (read-byte ,in nil #.(char-code #\Newline)))
                #+swank (read-char ,in nil #\Newline)
             for ,idx from 0
             until (char= ,character #\Newline)
             do (setf (schar ,buffer ,idx) ,character)
             finally (when (< ,idx ,buffer-size)
                       (setf (schar ,buffer ,idx) ,terminate-char))
                     (return (values ,buffer ,idx))))))
