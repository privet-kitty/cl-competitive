(defpackage :cp/date
  (:use :cl)
  (:export #:*month-days* #:leap-year-p #:get-day-of-week #:date-to-jdn #:jdn-to-date))
(in-package :cp/date)

(declaim ((simple-array (unsigned-byte 8) (*)) *month-days*))
(defparameter *month-days*
  #.(coerce #(31 28 31 30 31 30 31 31 30 31 30 31)
            '(simple-array (unsigned-byte 8) (*))))

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

(declaim (inline date-to-jdn))
(defun date-to-jdn (day month year)
  "Converts a Gregorian calendar date to Julian day number."
  (declare ((integer 1 31) day)
           ((integer 1 12) month)
           (fixnum year))
  (+ (truncate (* 1461 (+ year 4800 (truncate (- month 14) 12))) 4)
     (truncate (* 367 (- month 2 (* 12 (truncate (- month 14) 12)))) 12)
     (- (truncate (* 3 (truncate (+ year 4900 (truncate (- month 14) 12)) 100)) 4))
     day
     -32075))

;; Reference: https://forum.arduino.cc/index.php?topic=557576.msg3802719#msg3802719
(declaim (inline jdn-to-date)
         (ftype (function * (values (integer 1 31) (integer 1 12) integer &optional))
                jdn-to-date))
(defun jdn-to-date (jdn)
  "Converts a Julian day number to Gregotian calendar date."
  (declare ((integer -32044) jdn)) ;; broken under this value
  (let* ((l (+ jdn 68569))
         (n (truncate (* 4 l) 146097))
         (l (- l (truncate (+ (* 146097 n) 3) 4)))
         (year (truncate (* 4000 (+ l 1)) 1461001))
         (l (+ (- l (truncate (* 1461 year) 4)) 31))
         (month (truncate (* 80 l) 2447))
         (day (- l (truncate (* 2447 month) 80)))
         (l (truncate month 11))
         (month (- (+ month 2) (* 12 l)))
         (year (+ year l (* 100 (- n 49)))))
    (values day month year)))
