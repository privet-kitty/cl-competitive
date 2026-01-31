(defpackage :cp/test/multi-slope-trick
  (:use :cl :fiveam :cp/multi-slope-trick :cp/bisect :cp/shuffle)
  (:import-from :cp/multi-slope-trick #:%mstrick-base-slope #:%mstrick-intercept)
  (:import-from :cp/test/base #:base-suite))
(in-package :cp/test/multi-slope-trick)
(in-suite base-suite)

(defstruct (piecewise-linear (:constructor make-pl ())
                             (:conc-name %pl-)
                             (:copier nil)
                             (:predicate nil))
  "A naive representation of a piecewise linear convex function.
BREAKPOINTS is a sorted vector of x-coordinates where the slope changes.
SLOPES is a vector of length (1+ n) where n is the number of breakpoints.
SLOPES[i] is the slope of the function in the interval [BREAKPOINTS[i-1], BREAKPOINTS[i]).
INTERCEPT is the value of the function at x=0."
  (breakpoints (make-array 0 :element-type 'fixnum :adjustable t :fill-pointer 0)
   :type (vector fixnum))
  (slopes (make-array 1 :element-type 'fixnum :initial-element 0 :adjustable t :fill-pointer 1)
   :type (vector fixnum))
  (intercept 0 :type fixnum))

(defun pl-merge (pl)
  "Merge adjacent intervals with the same slope."
  (let ((breakpoints (%pl-breakpoints pl))
        (slopes (%pl-slopes pl)))
    (let ((new-bp (make-array 0 :element-type 'fixnum :adjustable t :fill-pointer 0))
          (new-slopes (make-array 0 :element-type 'fixnum :adjustable t :fill-pointer 0)))
      (vector-push-extend (aref slopes 0) new-slopes)
      (dotimes (i (length breakpoints))
        (let ((bp (aref breakpoints i))
              (slope (aref slopes (1+ i))))
          (when (/= slope (aref new-slopes (1- (length new-slopes))))
            (vector-push-extend bp new-bp)
            (vector-push-extend slope new-slopes))))
      (setf (%pl-breakpoints pl) new-bp
            (%pl-slopes pl) new-slopes)))
  pl)

(defun pl-value (pl x)
  "Compute the function value at X by integrating from x=0."
  (let ((breakpoints (%pl-breakpoints pl))
        (slopes (%pl-slopes pl))
        (intercept (%pl-intercept pl)))
    (cond
      ((zerop (length breakpoints))
       ;; No breakpoints, linear function
       (+ intercept (* (aref slopes 0) x)))
      (t
       ;; Find which segment x=0 is in and integrate to x
       (let* ((seg-0 (bisect-left breakpoints 0))
              (value intercept)
              (current-x 0))
         (cond
           ((>= x 0)
            ;; Integrate from 0 to x going right
            ;; Track the current segment index as we process breakpoints
            (let ((seg seg-0))
              (loop for i from seg-0 below (length breakpoints)
                    for bp = (aref breakpoints i)
                    while (< bp x)
                    do (incf value (* (aref slopes i) (- bp current-x)))
                       (setq current-x bp)
                       (setq seg (1+ i)))
              ;; Add final segment using tracked slope index
              (incf value (* (aref slopes seg) (- x current-x)))
              value))
           (t
            ;; Integrate from 0 to x going left (x < 0)
            ;; Track the current segment index as we process breakpoints
            (let ((seg seg-0))
              (when (> seg-0 0)
                (loop for i from (1- seg-0) downto 0
                      for bp = (aref breakpoints i)
                      while (> bp x)
                      do (decf value (* (aref slopes (1+ i)) (- current-x bp)))
                         (setq current-x bp)
                         (setq seg i)))
              ;; Subtract final segment using tracked slope index
              (decf value (* (aref slopes seg) (- current-x x)))
              value))))))))

(defun pl-add (pl a weight)
  "Adds max(0, weight*(x-a)) to f.
For weight > 0: slope increases by weight for x >= a.
For weight < 0: slope increases by weight for x < a."
  (when (zerop weight)
    (return-from pl-add pl))
  (let* ((breakpoints (%pl-breakpoints pl))
         (slopes (%pl-slopes pl))
         (index (bisect-left breakpoints a)))
    (unless (and (< index (length breakpoints))
                 (= a (aref breakpoints index)))
      ;; Insert breakpoint
      (vector-push-extend 0 breakpoints)
      (loop for i from (1- (length breakpoints)) downto (1+ index)
            do (setf (aref breakpoints i) (aref breakpoints (1- i))))
      (setf (aref breakpoints index) a)
      ;; Insert slope (copy from current interval)
      (vector-push-extend 0 slopes)
      (loop for i from (1- (length slopes)) downto (+ index 1)
            do (setf (aref slopes i) (aref slopes (1- i)))))
    (if (> weight 0)
        ;; Increase slopes for x >= a (that's slopes[pos+1] onwards)
        (loop for i from (1+ index) below (length slopes)
              do (incf (aref slopes i) weight))
        ;; Increase slopes for x < a (that's slopes[0..pos])
        (loop for i from 0 to index
              do (incf (aref slopes i) weight)))
    ;; Update intercept: max(0, weight*(x-a)) at x=0 is max(0, -weight*a)
    (incf (%pl-intercept pl) (max 0 (* (- weight) a)))
    (pl-merge pl)))

(defun pl-delete (pl a weight)
  "Subtracts max(0, weight*(x-a)) from f.
For weight > 0: slope decreases by weight for x >= a.
For weight < 0: slope decreases by weight for x < a.

The behaviour is undefined if the convexity is broken."
  (when (zerop weight)
    (return-from pl-delete pl))
  (let* ((breakpoints (%pl-breakpoints pl))
         (slopes (%pl-slopes pl))
         (index (bisect-left breakpoints a)))
    (unless (and (< index (length breakpoints))
                 (= a (aref breakpoints index)))
      ;; Insert breakpoint
      (vector-push-extend 0 breakpoints)
      (loop for i from (1- (length breakpoints)) downto (1+ index)
            do (setf (aref breakpoints i) (aref breakpoints (1- i))))
      (setf (aref breakpoints index) a)
      ;; Insert slope (copy from current interval)
      (vector-push-extend 0 slopes)
      (loop for i from (1- (length slopes)) downto (1+ index)
            do (setf (aref slopes i) (aref slopes (1- i)))))
    (if (> weight 0)
        ;; Decrease slopes for x >= a (that's slopes[index+1] onwards)
        (loop for i from (1+ index) below (length slopes)
              do (decf (aref slopes i) weight))
        ;; Decrease slopes for x < a (that's slopes[0..index])
        (loop for i from 0 to index
            do (decf (aref slopes i) weight)))
    ;; Update intercept: subtract max(0, -weight*a) (inverse of pl-add)
    (decf (%pl-intercept pl) (max 0 (* (- weight) a)))
    (pl-merge pl)))

(defun pl-add-abs (pl a weight)
  "Adds weight*|x-a| to f."
  (pl-add pl a weight)
  (pl-add pl a (- weight)))

(defun pl-add-linear (pl slope)
  "Adds a linear function x |-> slope*x to f."
  (let ((slopes (%pl-slopes pl)))
    (dotimes (i (length slopes))
      (incf (aref slopes i) slope)))
  pl)

(defun pl-left-cum (pl c)
  "g(x) = min_{t <= x} (f(t) - Ct) + Cx.
Clips slopes to (-infinity, C]."
  ;; g(0) = min_{t <= 0} (f(t) - Ct)
  ;; For convex f, minimum of f(t) - Ct is where slope_f = C
  ;; If slope at 0 <= C, then minimum for t <= 0 is at t = 0
  ;; Otherwise, find point t* <= 0 where slope = C
  (let ((new-intercept
          (multiple-value-bind (left-slope right-slope) (pl-subdiff pl 0)
            (declare (ignore left-slope))
            (if (<= right-slope c)
                ;; Slope at 0 is <= C, so minimum is at t = 0
                (%pl-intercept pl)
                ;; Find point where slope = C (it must be < 0)
                (multiple-value-bind (left right) (pl-arg-subdiff pl c)
                  (declare (ignore left))
                  (if (and (< +negative-inf+ right +positive-inf+)
                           (<= right 0))
                      (- (pl-value pl right) (* c right))
                      ;; Fallback: minimum at t = 0
                      (%pl-intercept pl)))))))
    (let ((breakpoints (%pl-breakpoints pl))
          (slopes (%pl-slopes pl)))
      (cond
        ;; If leftmost slope > c, make constant c
        ((> (aref slopes 0) c)
         (setf (fill-pointer breakpoints) 0)
         (setf (fill-pointer slopes) 1)
         (setf (aref slopes 0) c))
        (t
         ;; Find first breakpoint where slope becomes > c
         (let ((cut-index nil))
           (dotimes (i (length breakpoints))
             (when (> (aref slopes (1+ i)) c)
               (setq cut-index i)
               (return)))
           (when cut-index
             ;; Keep breakpoints[0..cut-index] and set last slope to c
             (setf (fill-pointer breakpoints) (1+ cut-index))
             (setf (fill-pointer slopes) (+ 2 cut-index))
             (setf (aref slopes (1+ cut-index)) c))))))
    (setf (%pl-intercept pl) new-intercept))
  (pl-merge pl))

(defun pl-right-cum (pl c)
  "g(x) = min_{x <= t} (f(t) - Ct) + Cx.
Clips slopes to [C, infinity)."
  ;; g(0) = min_{t >= 0} (f(t) - Ct)
  ;; For convex f, minimum of f(t) - Ct is where slope_f = C
  ;; If slope at 0 >= C, then minimum for t >= 0 is at t = 0
  ;; Otherwise, find point t* >= 0 where slope = C
  (let ((new-intercept
          (multiple-value-bind (left-slope right-slope) (pl-subdiff pl 0)
            (declare (ignore right-slope))
            (if (>= left-slope c)
                ;; Slope at 0 is >= C, so minimum is at t = 0
                (%pl-intercept pl)
                ;; Find point where slope = C (it must be > 0)
                (multiple-value-bind (left right) (pl-arg-subdiff pl c)
                  (declare (ignore right))
                  (if (and (< +negative-inf+ left +positive-inf+)
                           (>= left 0))
                      (- (pl-value pl left) (* c left))
                      ;; Fallback: minimum at t = 0
                      (%pl-intercept pl)))))))
    (let ((breakpoints (%pl-breakpoints pl))
          (slopes (%pl-slopes pl)))
      (cond
        ;; If rightmost slope < c, make constant c
        ((< (aref slopes (1- (length slopes))) c)
         (setf (fill-pointer breakpoints) 0)
         (setf (fill-pointer slopes) 1)
         (setf (aref slopes 0) c))
        (t
         ;; Find last breakpoint where slope BEFORE it is < c
         (let ((cut-index nil))
           (loop for i from (1- (length breakpoints)) downto 0
                 when (< (aref slopes i) c)
                 do (setq cut-index i)
                    (return))
           (when cut-index
             ;; Remove breakpoints 0..cut-index-1, keep from cut-index onwards
             (let ((keep-count (- (length breakpoints) cut-index)))
               (dotimes (j keep-count)
                 (setf (aref breakpoints j) (aref breakpoints (+ cut-index j))))
               (setf (fill-pointer breakpoints) keep-count)
               ;; Shift slopes similarly and set first to c
               (dotimes (j (1+ keep-count))
                 (setf (aref slopes j) (aref slopes (+ cut-index j))))
               (setf (fill-pointer slopes) (1+ keep-count))
               (setf (aref slopes 0) c)))))))
    (setf (%pl-intercept pl) new-intercept))
  (pl-merge pl))

(defun pl-shift (pl ldelta &optional rdelta)
  "g(x) = min_{x-rdelta <= t <= x-ldelta} f(t).
Shifts left breakpoints (negative slope before) by ldelta,
shifts right breakpoints (positive slope after) by rdelta."
  (let ((rdelta (or rdelta ldelta)))
    (assert (<= ldelta rdelta))
    ;; Compute new intercept: g(0) = min_{-rdelta <= t <= -ldelta} f(t)
    ;; For convex f, minimum over [a, b] is at:
    ;; - a if slope at a >= 0
    ;; - b if slope at b <= 0
    ;; - point where slope = 0 if it's in [a, b]
    (let* ((left-bound (- rdelta))
           (right-bound (- ldelta)))
      (if (= left-bound right-bound)
          ;; Uniform shift: g(0) = f(-delta)
          (setf (%pl-intercept pl) (pl-value pl left-bound))
          ;; Find minimum over interval
          (multiple-value-bind (slope-left-l slope-left-r) (pl-subdiff pl left-bound)
            (declare (ignore slope-left-l))
            (multiple-value-bind (slope-right-l slope-right-r) (pl-subdiff pl right-bound)
              (declare (ignore slope-right-r))
              (setf (%pl-intercept pl)
                    (cond
                      ;; Minimum at right bound (slope <= 0 throughout interval)
                      ((<= slope-right-l 0)
                       (pl-value pl right-bound))
                      ;; Minimum at left bound (slope >= 0 throughout interval)
                      ((>= slope-left-r 0)
                       (pl-value pl left-bound))
                      ;; Minimum where slope = 0
                      (t
                       (multiple-value-bind (left right) (pl-arg-subdiff pl 0)
                         (cond
                           ((and left (<= left-bound left) (<= left right-bound))
                            (pl-value pl left))
                           ((and right (<= left-bound right) (<= right right-bound))
                            (pl-value pl right))
                           (t
                            ;; Fallback
                            (min (pl-value pl left-bound) (pl-value pl right-bound))))))))))))
    ;; Update breakpoints
    (let ((breakpoints (%pl-breakpoints pl))
          (slopes (%pl-slopes pl)))
      (cond
        ((zerop (length breakpoints))
         ;; No breakpoints, nothing to shift
         nil)
        ((= ldelta rdelta)
         ;; Simple case: shift all breakpoints uniformly
         (dotimes (i (length breakpoints))
           (incf (aref breakpoints i) ldelta)))
        (t
         ;; Complex case: different shifts for left and right
         (let ((new-bp (make-array 0 :element-type 'fixnum :adjustable t :fill-pointer 0))
               (new-slopes (make-array 0 :element-type 'fixnum :adjustable t :fill-pointer 0)))
           (vector-push-extend (aref slopes 0) new-slopes)
           (dotimes (i (length breakpoints))
             (let ((left-slope (aref slopes i))
                   (right-slope (aref slopes (1+ i)))
                   (bp (aref breakpoints i)))
               (cond
                 ;; Only left: slope before is negative, slope after is non-positive
                 ((and (< left-slope 0) (<= right-slope 0))
                  (vector-push-extend (+ bp ldelta) new-bp)
                  (vector-push-extend right-slope new-slopes))
                 ;; Only right: slope before is non-negative, slope after is positive
                 ((and (>= left-slope 0) (> right-slope 0))
                  (vector-push-extend (+ bp rdelta) new-bp)
                  (vector-push-extend right-slope new-slopes))
                 ;; Both: slope before is negative, slope after is positive
                 ;; Split into two breakpoints with a flat region between
                 ((and (< left-slope 0) (> right-slope 0))
                  (vector-push-extend (+ bp ldelta) new-bp)
                  (vector-push-extend 0 new-slopes)
                  (vector-push-extend (+ bp rdelta) new-bp)
                  (vector-push-extend right-slope new-slopes))
                 ;; Neither: slope before is 0, slope after is 0
                 (t
                  (vector-push-extend (+ bp ldelta) new-bp)
                  (vector-push-extend right-slope new-slopes)))))
           (setf (%pl-breakpoints pl) new-bp
                 (%pl-slopes pl) new-slopes)
           (pl-merge pl))))))
  pl)

(defun pl-arg-subdiff (pl diff)
  "Returns the interval [left, right] where the subdifferential contains DIFF.
Returns [-inf, -inf] if DIFF is below every slope, [+inf, +inf] if above."
  (let ((breakpoints (%pl-breakpoints pl))
        (slopes (%pl-slopes pl)))
    (let ((base-slope (aref slopes 0))
          (end-slope (aref slopes (1- (length slopes)))))
      (cond ((< diff base-slope) (values +negative-inf+ +negative-inf+))
            ((< end-slope diff) (values +positive-inf+ +positive-inf+))
            (t
             (let ((left-end (if (= diff base-slope)
                                 +negative-inf+
                                 nil))
                   (right-end (if (= diff end-slope)
                                  +positive-inf+
                                  nil)))
               ;; Find left-end: first breakpoint where slope becomes >= diff
               (unless left-end
                 (loop for i from 0 below (length breakpoints)
                       when (and (< (aref slopes i) diff)
                                 (>= (aref slopes (1+ i)) diff))
                       do (setq left-end (aref breakpoints i))
                          (return)))
               ;; Find right-end: last breakpoint where slope becomes > diff
               (unless right-end
                 (loop for i from (1- (length breakpoints)) downto 0
                       when (and (<= (aref slopes i) diff)
                                 (> (aref slopes (1+ i)) diff))
                       do (setq right-end (aref breakpoints i))
                          (return)))
               (values left-end right-end)))))))

(defun pl-subdiff (pl x)
  "Returns the subdifferential at x as (values left-slope right-slope)."
  (let ((breakpoints (%pl-breakpoints pl))
        (slopes (%pl-slopes pl)))
    ;; Find the interval containing x
    (let ((index (bisect-left breakpoints x)))
      (if (and (< index (length breakpoints))
               (= x (aref breakpoints index)))
          (values (aref slopes index) (aref slopes (1+ index)))
          (values (aref slopes index) (aref slopes index))))))

(test slope-trick-operation/random
  (let ((*random-state* (sb-ext:seed-random-state 0))
        (*test-dribble* nil))
    (dotimes (_ 10000)
      (let* ((base-slope (- (random 10) 5))
             (mstrick (make-multi-slope-trick base-slope))
             (pl (make-pl))
             (add-history nil))
        (pl-add-linear pl base-slope)
        (dotimes (i 100)
          (ecase (random 16)
            ((0 1 2 3 4 5)
             ;; add
             (let ((a (- (random 20) 10))
                   (weight (- (random 20) 10)))
               (mstrick-add mstrick a weight)
               (pl-add pl a weight)
               (push (cons a weight) add-history)))
            ((6)
             ;; add-linear
             (let ((slope (- (random 20) 10)))
               (mstrick-add-linear mstrick slope)
               (pl-add-linear pl slope)))
            ((7)
             ;; shift operation
             (let* ((ldelta (- (random 20) 15))
                    (rdelta (+ ldelta (random 10))))
               (mstrick-shift mstrick ldelta rdelta)
               (pl-shift pl ldelta rdelta)
               (setq add-history nil)))
            ((8 9)
             (let ((rollback-p (zerop (random 3))))
               (if (zerop (random 2))
                   (let* ((c (+ (%mstrick-base-slope mstrick) (random 15)))
                          (rest-part (mstrick-left-cum mstrick c)))
                     (if rollback-p
                         (mstrick-left-cum-rollback mstrick rest-part)
                         (progn
                           ;; (format t "~%left-cum: ~D" c)
                           (pl-left-cum pl c)
                           (setq add-history nil))))
                   (let* ((slopes (%pl-slopes pl))
                          (c (- (aref slopes (- (length slopes) 1))
                                (random 15)))
                          (rest-part (mstrick-right-cum mstrick c)))
                     (if rollback-p
                         (mstrick-right-cum-rollback mstrick rest-part)
                         (progn
                           ;; (format t "~%right-cum: ~D" c)
                           (pl-right-cum pl c)
                           (setq add-history nil)))))))
            ((10 11)
             ;; check function value
             (let ((xs (subseq (shuffle! (coerce (loop for x from -12 to 12 collect x)
                                                 'vector))
                               0 (+ 1 (random 15)))))
               (is-true
                (loop for x across xs
                      always (= (mstrick-value mstrick x)
                                (pl-value pl x))))))
            ((12 13)
             ;; check subdiff and arg-subdiff
             (let ((xs (subseq (shuffle! (coerce (loop for x from -21 to 21 collect x)
                                                 'vector))
                               0 10)))
               (is-true
                (loop for x across xs
                      always (equal (multiple-value-list (mstrick-subdiff mstrick x))
                                    (multiple-value-list (pl-subdiff pl x))))))
             (let ((diffs (subseq (shuffle! (coerce (loop for x from -21 to 21 collect x)
                                                    'vector))
                                  0 10)))
               (is-true
                (loop for diff across diffs
                      always (equal (multiple-value-list (mstrick-arg-subdiff mstrick diff))
                                    (multiple-value-list (pl-arg-subdiff pl diff)))))))
            ((14 15)
             ;; delete (rollback a random add from history)
             (when add-history
               (let* ((idx (random (length add-history)))
                      (entry (nth idx add-history))
                      (a (car entry))
                      (weight (cdr entry)))
                 ;; (format t "~%del: ~D ~D" a weight)
                 (mstrick-delete mstrick a weight)
                 (pl-delete pl a weight)
                 (setq add-history
                       (nconc (subseq add-history 0 idx)
                              (nthcdr (+ idx 1) add-history))))))))))))

(defun test-hand ()
  (let ((*random-state* (sb-ext:seed-random-state 2)))
    (let ((ms (make-multi-slope-trick -5)))
      (print ms)
      (mstrick-add ms 4 -1)
      (print ms)
      (mstrick-add ms 3 1)
      (print ms)
      (mstrick-shift ms -3 -3)
      (mstrick-value ms 10))))
