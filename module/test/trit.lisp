(defpackage :cp/test/trit
  (:use :cl :fiveam :cp/trit)
  (:import-from :cp/test/base #:base-suite))
(in-package :cp/test/trit)
(in-suite base-suite)

(test logtrit/hand
  (let* ((str "201021")
         (len (length str))
         (int (parse-integer str :radix 3)))
    (dotimes (i (length str))
      (is (= (digit-char-p (aref str (- (length str) i 1)))
             (logtrit i int))))
    (dotimes (l (+ 1 len))
      (loop for r from l to (+ len 3)
            do (is (= (ldt (byte (- r l) l) int)
                      (if (or (= l len) (= l r))
                          0
                          (parse-integer str
                                         :radix 3
                                         :start (max 0 (- len r))
                                         :end (- len l)))))))))

(test logtrit/random
  (let ((*test-dribble* nil)
        (*random-state* (sb-ext:seed-random-state 0)))
    (dotimes (len 8)
      (let* ((len 6)
             (str (make-string len :initial-element #\0))
             (int 0))
        (dotimes (_ 100)
          (let ((pos (random len))
                (new (random 3)))
            (setf (aref str (- len pos 1)) (code-char (+ 48 new))
                  (logtrit pos int) new)
            (is (= int (parse-integer str :radix 3))))
          (let ((pos (random len)))
            (is (= (digit-char-p (aref str (- len pos 1)))
                   (logtrit pos int)))))))))
