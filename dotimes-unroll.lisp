(defmacro dotimes-unroll ((var count size &optional result) &body body)
  "DOTIMES macro with loop-unrolling by SIZE."
  (let ((base (gensym))
        (whole (gensym))
        (_count (gensym)))
    (check-type var symbol)
    (check-type size (integer 1 #.most-positive-fixnum))
    `(block ,whole
       (let ((,_count ,count)
             (,base 0))
         (declare ((integer 0 #.most-positive-fixnum) ,base ,_count))
         (loop
           (when (> (+ ,base ,size) ,_count)
             (do ((,var ,base (1+ ,var)))
                 ((>= ,var ,_count)
                  (return-from ,whole ,result))
               ,@body))
           ,@(loop for i from 0 below size
                   collect `(let ((,var (+ ,base ,i))) ,@body))
           (setq ,base (+ ,base ,size)))))))

(defmacro dotimes-unroll-all ((var count &optional result) &body body)
  (check-type count (integer 0))
  `(block nil
     ,@(loop for i from 0 below count
             collect `(let ((,var ,i)) ,@body))
     ,result))
