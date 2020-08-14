(defpackage :cp/trisect
  (:use :cl)
  (:export #:trisect-left))
(in-package :cp/trisect)

;; FIXME: This is actually not a trisection and should be renamed.
(declaim (inline trisect-left))
(defun trisect-left (target start end &key (order #'<))
  "TARGET := function | (TODO: vector)

Returns the leftmost index at which TARGET takes the minimum. TARGET must
satisfy the following condition: the comparison of TARGET[i] and TARGET[i+1] are
monotone, i.e. >, >, ..., >, =, =, ..., =, <, <, ..., < for i = START, START+1,
..., END-1; convexity or unimodality is sufficient. (If order is #'>, these
behaviours are so replaced and this function detects the maximum.)

Note that this function searches the optimal value in the **closed** interval
[START, END]."
  (when (>= start end)
    (assert (= start end))
    (return-from trisect-left start))
  (let ((ng (- start 1))
        (ok end))
    (loop
      (if (<= (- ok ng) 1)
          (return ok)
          (let ((mid (ash (+ ok ng) -1)))
            (if (funcall order
                         (funcall target (+ 1 mid))
                         (funcall target mid))
                (setq ng mid)
                (setq ok mid)))))))
