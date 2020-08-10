(eval-when (:compile-toplevel :load-toplevel :execute)
  (load "test-util")
  (load "../date.lisp"))

(use-package :test-util)

(defun get-day-of-week2 (day month year)
  (declare (fixnum day month year))
  (when (< year 2800)
    (incf year (* 2800 (ceiling (- 2800 year) 2800))))
  (mod (+ 1 (nth-value
             6
             (decode-universal-time
              (encode-universal-time 0 0 0 day month year 0)
              0)))
       7))

(with-test (:name get-day-of-week)
  (loop
    for year from -3000 to 3000
    do (loop
         for month from 1 to 12
         do (loop
              for day from 1 to (aref (if (leap-year-p year)
                                          #(-1 31 29 31 30 31 30 31 31 30 31 30 31)
                                          #(-1 31 28 31 30 31 30 31 31 30 31 30 31))
                                      month)
              do (assert (= (get-day-of-week day month year)
                            (get-day-of-week2 day month year)))))))
