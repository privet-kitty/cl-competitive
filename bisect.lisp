(declaim (inline bisect-left))
(defun bisect-left (target value &key (start 0) end (predicate #'<) (key #'identity))
  "TARGET := vector | function
PREDICATE := strict order

Returns the smallest index (or input) i that fulfills TARGET[i] >= VALUE, where
'>=' is the complement of PREDICATE. TARGET must be monotonically non-decreasing with
respect to PREDICATE. This function returns END if VALUE exceeds TARGET[END-1]. Note
that the range [START, END) is half-open. END must be explicitly specified If
TARGET is function. KEY is applied to each element of TARGET before comparison."
  (declare (function key predicate)
           ((integer 0 #.most-positive-fixnum) start)
           ((or null (integer 0 #.most-positive-fixnum)) end))
  (macrolet
      ((body (accessor &optional declaration)
         `(cond ((assert (<= start end)))
                ((= start end) end)
                ((funcall predicate (funcall key (,accessor target (- end 1))) value)
                 end)
                (t (labels ((%bisect-left (l r)
                              ,@(list declaration)
                              (let ((mid (ash (+ l r) -1)))
                                (if (= mid l)
                                    (if (funcall predicate (funcall key (,accessor target l)) value)
                                        r
                                        l)
                                    (if (funcall predicate (funcall key (,accessor target mid)) value)
                                        (%bisect-left mid r)
                                        (%bisect-left l mid))))))
                     (%bisect-left start (- end 1)))))))
    (etypecase target
      (vector
       (let ((end (or end (length target))))
         (body aref (declare ((integer 0 #.most-positive-fixnum) l r)))))
      (function
       (assert end)
       (body funcall)))))

(declaim (inline bisect-right))
(defun bisect-right (target value &key (start 0) end (predicate #'<) (key #'identity))
  "TARGET := vector | function
PREDICATE := strict order

Returns the smallest index (or input) i that fulfills TARGET[i] > VALUE. TARGET
must be monotonically non-decreasing with respect to PREDICATE. This function returns
END if VALUE exceeds TARGET[END-1]. Note that the range [START, END) is
half-open. END must be explicitly specified if TARGET is function. KEY is
applied to each element of TARGET before comparison."
  (declare (function key predicate)
           ((integer 0 #.most-positive-fixnum) start)
           ((or null (integer 0 #.most-positive-fixnum)) end))
  (macrolet
      ((body (accessor &optional declaration)
         `(cond ((assert (<= start end)))
                ((= start end) end)
                ((funcall predicate value (funcall key (,accessor target (- end 1))))
                 (labels ((%bisect-right (l r)
                            ,@(list declaration)
                            (let ((mid (ash (+ l r) -1)))
                              (if (= mid l)
                                  (if (funcall predicate value (funcall key (,accessor target l)))
                                      l
                                      r)
                                  (if (funcall predicate value (funcall key (,accessor target mid)))
                                      (%bisect-right l mid)
                                      (%bisect-right mid r))))))
                   
                   (%bisect-right start (- end 1))))
                (t end))))
    (etypecase target
      (vector
       (when (null end)
         (setf end (length target)))
       (body aref (declare ((integer 0 #.most-positive-fixnum) l r))))
      (function
       (assert end)
       (body funcall)))))
