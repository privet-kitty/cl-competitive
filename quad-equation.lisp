;; TODO: handling for the cases a == 0
(declaim (inline solve-quad-equation))
(defun solve-quad-equation (a b c)
  "Solves ax^2 + bx + c = 0"
  (let* ((a (float a 1d0))
         (b (float b 1d0))
         (c (float c 1d0))
         (d (sqrt (- (* b b) (* 4 a c)))))
    (handler-bind ((arithmetic-error
                     (lambda (_)
                       (declare (ignore _))
                       (return-from solve-quad-equation
                         (values (/ (+ (- b) d) (* 2 a))
                                 (/ (- d b) (* 2 a)))))))
      (let ((result1 (/ (+ (abs b) d) (* 2 a))))
        (if (< b 0)
            (values result1 (/ c (* a result1)))
            (values (- result1) (/ c (* a (- result1)))))))))
