(defmacro buffered-read-line (&optional (buffer-size 30) (in '*standard-input*) (terminate-char #\Space))
  "Receives ascii inputs and returns the string and the end position. Note that
the returned string will be reused.

This macro calls READ-BYTE to read characters though it calls READ-CHAR instead
on SLIME because SLIME's IO is not bivalent."
  (let ((buffer (gensym))
        (character (gensym))
        (idx (gensym)))
    `(let* ((,buffer (load-time-value (make-string ,buffer-size :element-type 'base-char))))
       (declare (simple-base-string ,buffer)
                (inline read-byte))
       (loop for ,character of-type base-char =
                ,(if (member :swank *features*)
                     `(read-char ,in nil #\Newline) ; on SLIME
                     `(code-char (read-byte ,in nil #.(char-code #\Newline))))
             for ,idx from 0
             until (char= ,character #\Newline)
             do (setf (schar ,buffer ,idx) ,character)
             finally (when (< ,idx ,buffer-size)
                       (setf (schar ,buffer ,idx) ,terminate-char))
                     (return (values ,buffer ,idx))))))
