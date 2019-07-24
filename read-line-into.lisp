(declaim (inline read-line-into))
(defun read-line-into (buffer-string &key (in *standard-input*) (term-char #\Space))
  "Receives ASCII inputs and returns multiple values: the string and the end
position.

This function calls READ-BYTE to read characters though it calls READ-CHAR
instead on SLIME because SLIME's IO is not bivalent."
  (declare (inline read-byte)) ; declaring (sb-kernel:ansi-stream in) will be faster
  (loop for c of-type base-char =
           #-swank (code-char (read-byte in nil #.(char-code #\Newline)))
           #+swank (read-char in nil #\Newline)
        for idx from 0
        until (char= c #\Newline)
        do (setf (char buffer-string idx) c)
        finally (when (< idx (length buffer-string))
                  (setf (char buffer-string idx) term-char))
                (return (values buffer-string idx))))
