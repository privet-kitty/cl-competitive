;; Just for an experiment. Use SB-INT:POSITIVE-PRIME-P.

(declaim (inline divide-by-2s))
(defun divide-by-2s (num)
  (do ((n num (ash n -1))
       (s 0 (1+ s)))
      ((oddp n) (values s n))))

(defun primep (n)
  (let ((n-1 (- n 1)))
    (if (or (evenp n) (<= n 1))
        (if (= n 2) t nil)
        (multiple-value-bind (s d)
            (divide-by-2s n-1)
          (loop for a from 2 to (min (- n 1)
                                     (floor (* 2 (expt (log n) 2))))
             for ad = (expt a d)
             do (when
                    (and (/= (mod ad n) 1)
                         (loop for r from 0 below s
                            for two-r = 1 then (+ two-r two-r)
                            do (when (= n-1
                                        (mod (expt ad two-r) n))
                                 (return nil))
                            finally (return t)))
                  (return nil))
             finally (return t))))))


;; n < 4759123141
(defun primep (n)
  (declare (optimize (speed 3) (safety 0))
           ((integer 0 4759123140) n))
  (labels ((divide-by-2s (num)
             (do ((n num (ash n -1))
                  (s 0 (1+ s)))
                 ((oddp n) (values s n))
               (declare (fixnum n s)))))
    (declare (inline divide-by-2s))
    (let ((n-1 (- n 1)))
      (if (or (evenp n) (<= n 1))
          (if (= n 2) t nil)
          (multiple-value-bind (s d)
              (divide-by-2s n-1)
            (loop for a of-type (integer 2 61) in '(2 7 61)
                  for ad = (expt a d)
                  do (when (and (/= (mod ad n) 1)
                                (loop for r from 0 below s
                                      for two-r of-type fixnum = 1 then (+ two-r two-r)
                                      do (when (= n-1 (mod (expt ad two-r) n))
                                           (return nil))
                                      finally (return t)))
                       (return nil))
                  finally (return t)))))))

