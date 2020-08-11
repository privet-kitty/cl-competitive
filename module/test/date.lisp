(defpackage :cp/test/date
  (:use :cl :fiveam :cp/date)
  (:import-from :cp/test/base #:base-suite))
(in-package :cp/test/date)
(in-suite base-suite)

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

(test get-day-of-week
  (finishes
    (loop
      for year from -1000 to 2000
      do (loop
           for month from 1 to 12
           do (loop
                for day from 1 to (aref (if (leap-year-p year)
                                            #(-1 31 29 31 30 31 30 31 31 30 31 30 31)
                                            #(-1 31 28 31 30 31 30 31 31 30 31 30 31))
                                        month)
                do (assert (= (get-day-of-week day month year)
                              (get-day-of-week2 day month year))))))))
