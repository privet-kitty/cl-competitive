;; This is actually not a trisection but a bisection though I put this here as
;; the goal is the same; it is an alternative of ternary search on an integer
;; function.
(declaim (inline trisect-left))
(defun trisect-left (target start end &key maximum)
  "TARGET := function | (TODO: vector)

Returns the leftmost index at which TARGET takes the minimum. TARGET must
satisfy the following condition: the signs of TARGET[i+1] - TARGET[i] are
monotone, i.e. -, -, ..., -, 0, 0, ..., 0, +, +, ..., + for i = START, START+1,
..., END-1; convexity or unimodality is sufficient. (If MAXIMUM is true, these
behaviours are so replaced.)

Note that this function searches the optimal value in the **closed** interval
[START, END]."
  (when (>= start end)
    (assert (= start end))
    (return-from trisect-left start))
  (let ((ng (- start 1))
        (ok end))
    (if maximum
        (loop
          (if (<= (- ok ng) 1)
              (return ok)
              (let ((mid (ash (+ ok ng) -1)))
                (if (<= (funcall target (+ 1 mid))
                        (funcall target mid))
                    (setq ok mid)
                    (setq ng mid)))))
        (loop
          (if (<= (- ok ng) 1)
              (return ok)
              (let ((mid (ash (+ ok ng) -1)))
                (if (>= (funcall target (+ 1 mid))
                        (funcall target mid))
                    (setq ok mid)
                    (setq ng mid))))))))
