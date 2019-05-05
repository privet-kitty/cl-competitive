;;;
;;; GARBAGE
;;;

(declaim (inline backward-parse-integer))
(defun backward-parse-integer (string &key (end (length string)) (declare-fixnum t))
  (declare ((integer 1 #.most-positive-fixnum) end))
  (if declare-fixnum
      (loop for idx from (- end 1) downto 0
            for digit = (- (char-code (aref string idx)) 48)
            while (<= 0 digit 9)
            for base of-type (integer 1 #.(expt 10 18)) = 1 then (* base 10)
            sum (* base digit) into res of-type fixnum
            finally (if (= digit #.(- (char-code #\-) 48))
                        (return (values (- res) (max 0 (- idx 1))))
                        (return (values res (max 0 idx)))))
      (loop for idx from (- end 1) downto 0
            for digit = (- (char-code (aref string idx)) 48)
            while (<= 0 digit 9)
            for base of-type integer = 1 then (* base 10)
            sum (* base digit) into res of-type integer
            finally (if (= digit #.(- (char-code #\-) 48))
                        (return (values (- res) (max 0 (- idx 1))))
                        (return (values res (max 0 idx)))))))

(defmacro split-ints-and-bind* (vars values-string-pos &body body)
  (let ((pos1 (gensym "POS"))
        (pos2 (gensym "POS"))
        (str (gensym "STR")))
    (labels ((expand (vars &optional (init-pos t))
               (if (null vars)
                   body
                   `((let ((,pos1 ,(if init-pos `,pos1 `,pos2)))
                       (multiple-value-bind (,(car vars) ,pos2)
                           (backward-parse-integer ,str :end ,pos1)
                         (declare (ignorable ,pos2))
                         ,@(expand (cdr vars) nil)))))))
      `(multiple-value-bind (,str ,pos1) ,values-string-pos
         ,@(expand (reverse vars))))))
