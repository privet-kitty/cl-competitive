(defpackage :cp/date
  (:use :cl)
  (:export #:leap-year-p #:get-day-of-week #:get-julian-day-number))
(in-package :cp/date)

(declaim (inline leap-year-p))
(defun leap-year-p (year)
  (or (zerop (mod year 400))
      (and (zerop (mod year 4))
           (not (zerop (mod year 100))))))

;; Gauss's algorithm
;; https://en.wikipedia.org/wiki/Determination_of_the_day_of_the_week
(defun get-day-of-week (day month year)
  "Returns the day of the week (starting with Sunday) as an integer from 0 to 6.
Note that DAY and MONTH are 1-based."
  (declare ((integer 1 31) day)
           ((integer 1 12) month)
           (fixnum year))
  (let ((ms #.(coerce #(0 3 3 6 1 4 6 2 5 0 3 5)
                      '(simple-array (unsigned-byte 4) (*))))
        (leap-ms #.(coerce #(0 3 4 0 2 5 0 3 6 1 4 6)
                           '(simple-array (unsigned-byte 4) (*)))))
    (let ((m (aref (if (leap-year-p year) leap-ms ms)
                   (- month 1))))
      (declare ((integer 0 6) m))
      (mod (+ day
              m
              (* 5 (mod (- year 1) 4))
              (* 4 (mod (- year 1) 100))
              (* 6 (mod (- year 1) 400)))
           7))))

(declaim (inline date-to-jnd))
(defun date-to-jnd (day month year)
  "Converts a Gregorian calendar date to Julian day number. (It will be used to
get the number of days between two dates."
  (+ (truncate (* 1461 (+ year 4800 (truncate (- month 14) 12))) 4)
     (truncate (* 367 (- month 2 (* 12 (truncate (- month 14) 12)))) 12)
     (- (truncate (* 3 (truncate (+ year 4900 (truncate (- month 14) 12)) 100)) 4))
     day
     -32075))
