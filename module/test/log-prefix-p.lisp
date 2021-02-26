(defpackage :cp/test/log-prefix-p
  (:use :cl :fiveam :cp/log-prefix-p :cp/prefix-p)
  (:import-from :cp/test/base #:base-suite))
(in-package :cp/test/log-prefix-p)
(in-suite base-suite)

(test log-prefix-p/hand
  (is (log-prefix-p 0 0))
  (is (log-prefix-p 0 1))
  (is (not (log-prefix-p 1 #b10)))
  (is (log-prefix-p 1 #b11))
  (is (log-prefix-p #b101 #b10101))
  (is (not (log-prefix-p #b10101 #b101))))

(defun to-string (b)
  (if (zerop b)
      (make-string 0 :element-type 'base-char)
      (nreverse (write-to-string b :base 2))))

(test log-prefix-p/random
  (let ((*test-dribble* nil)
        (*random-state* (sb-ext:seed-random-state 0)))
    (dotimes (_ 10000)
      (let* ((b1 (random 256))
             (b2 (random 256))
             (s1 (to-string b1))
             (s2 (to-string b2)))
        (is (eql (prefix-p s1 s2) (log-prefix-p b1 b2)))
        (is (eql (log-prefix-p b1 b2) (prefix-p s1 s2)))))))
