(defpackage :cp/with-progressive-timeout
  (:use :cl)
  (:export #:with-progressive-timeout))
(in-package :cp/with-progressive-timeout)

;; NOTE: not tested
(defmacro with-progressive-timeout ((name seconds
                                     &key per-count simulate-count (type 'double-float))
                                    &body body)
  "Defines a local function with NAME, which returns the remaining time measured
from the time this block is entered. NAME returns 0.0 after SECONDS have
passed.

If PER-COUNT is given, NAME only calls GET-INTERNAL-REAL-TIME once per it.

This macro is an extension of SB-INT:WITH-PROGRESSIVE-TIMEOUT."
  (check-type per-count (or null (integer 1)))
  (check-type simulate-count (or null (integer 1)))
  (check-type type (member short-float single-float double-float long-float))
  (let ((per-count (or per-count 1))
        (deadline (gensym "DEADLINE"))
        (time-left (gensym "DEADLINE"))
        (count (gensym "COUNT"))
        (prev (gensym "PREV"))
        (denom (gensym "DENOM"))
        (sec (gensym "SEC")))
    (if simulate-count
        `(let* ((,count 0)
                (,sec (coerce ,seconds ',type))
                (,denom (coerce ,simulate-count ',type)))
           (declare ((integer 0 #.most-positive-fixnum) ,count)
                    (,type ,denom))
           (labels ((,name ()
                      (prog1
                          (coerce (* ,sec (/ (- ,simulate-count ,count) ,denom))
                                  ',type)
                        (incf ,count))))
             ,@body))
        `(let* ((,count 0)
                (,deadline
                  (+ (get-internal-real-time)
                     (round (* ,seconds internal-time-units-per-second))))
                (,prev (coerce ,seconds ',type)))
           (declare ((mod ,per-count) ,count)
                    ((integer 0 #.most-positive-fixnum) ,deadline))
           (labels ((,name ()
                      (setq ,count (mod (1+ ,count) ,per-count))
                      (if (zerop ,count)
                          (setq ,prev
                                (let ((,time-left (- ,deadline (get-internal-real-time))))
                                  (if (plusp ,time-left)
                                      (* (coerce ,time-left ',type)
                                         ,(/ (coerce 1 type)
                                             internal-time-units-per-second))
                                      ,(coerce 0 type))))
                          ,prev)))
             ,@body)))))
