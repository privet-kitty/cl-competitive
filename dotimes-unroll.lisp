(defmacro dotimes-unroll ((var count &optional result) &body body)
  `(block nil
     ,@(loop for i from 0 below count
             collect `(let ((,var ,i)) ,@body))
     ,result))
