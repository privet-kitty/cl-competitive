;; This is a makeshift implementation of float parser. It doesn't serve a
;; serious purpose.
(defun parse-double-float (string &key (start 0) end (declare-fixnum t))
  (declare ((integer 0 #.most-positive-fixnum) start)
           (simple-base-string string))
  (let* ((end (or end (length string)))
         (minus nil)
         (point-pos -1)
         (start (loop for idx from start below end
                      do (let ((code (char-code (aref string idx))))
                           (cond ((= code #.(char-code #\-))
                                  (setq minus t)
                                  (return (1+ idx)))
                                 ((<= 48 code 57)
                                  (return idx))
                                 ((= code #.(char-code #\.))
                                  (setf point-pos idx)
                                  (return idx))
                                 ((= code #.(char-code #\+))
                                  (return (1+ idx)))))
                      finally (error "No digits found."))))
    (declare ((integer 1 #.most-positive-fixnum) end))
    (if declare-fixnum
        (loop with res of-type (integer 0 #.most-positive-fixnum) = 0
              for idx from start below end
              for code = (char-code (aref string idx))
              do (cond ((<= 48 code 57)
                        (setq res (+ (- code 48) (the fixnum (* res 10)))))
                       ((= code #.(char-code #\.))
                        (setq point-pos idx))
                       (t (loop-finish)))
              finally
                 (let ((float (* res
                                 (if (= point-pos -1)
                                     1d0
                                     (aref #.(coerce #(1d0 1d-1 1d-2 1d-3 1d-4 1d-5 1d-6 1d-7 1d-8 1d-9 1d-10 1d-11 1d-12 1d-13 1d-14 1d-15 1d-16 1d-17 1d-18 1d-19 1d-20 1d-21 1d-22 1d-23 1d-24 1d-25 1d-26 1d-27 1d-28 1d-29 1d-30 1d-31 1d-32 1d-33 1d-34 1d-35 1d-36 1d-37 1d-38 1d-39 1d-40 1d-41 1d-42 1d-43 1d-44 1d-45 1d-46 1d-47 1d-48 1d-49)
            '(simple-array double-float (*)))
                                           (- idx point-pos 1))))))
                   (declare (double-float float))
                   (return (values (if minus (- float) float)
                                   idx))))
        (error "Not implemented."))))

