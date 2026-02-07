(defpackage :cp/test/multi-slope-trick-float
  (:use :cl :fiveam :cp/multi-slope-trick-float :cp/bisect :cp/shuffle)
  (:import-from :cp/multi-slope-trick-float #:%mstrick-base-slope #:%mstrick-intercept)
  (:import-from :cp/test/base #:base-suite))
(in-package :cp/test/multi-slope-trick-float)
(in-suite base-suite)

(defun approx= (a b &optional (eps 1d-4))
  (declare (double-float a b eps))
  (or (= a b) (<= (abs (- a b)) eps)))

(defun key< (a b)
  (float< a b +key-eps+))

(defstruct (piecewise-linear (:constructor make-pl ())
                             (:conc-name %pl-)
                             (:copier nil)
                             (:predicate nil))
  "A naive representation of a piecewise linear convex function.
BREAKPOINTS is a sorted vector of x-coordinates where the slope changes.
SLOPES is a vector of length (1+ n) where n is the number of breakpoints.
SLOPES[i] is the slope of the function in the interval [BREAKPOINTS[i-1], BREAKPOINTS[i]).
INTERCEPT is the value of the function at x=0."
  (breakpoints (make-array 0 :element-type 'double-float :adjustable t :fill-pointer 0)
   :type (vector double-float))
  (slopes (make-array 1 :element-type 'double-float :initial-element 0d0 :adjustable t :fill-pointer 1)
   :type (vector double-float))
  (intercept 0d0 :type double-float))

(defun pl-merge (pl)
  "Merge adjacent intervals with the same slope."
  (let ((breakpoints (%pl-breakpoints pl))
        (slopes (%pl-slopes pl)))
    (let ((new-bp (make-array 0 :element-type 'double-float :adjustable t :fill-pointer 0))
          (new-slopes (make-array 0 :element-type 'double-float :adjustable t :fill-pointer 0)))
      (vector-push-extend (aref slopes 0) new-slopes)
      (dotimes (i (length breakpoints))
        (let ((bp (aref breakpoints i))
              (slope (aref slopes (1+ i))))
          (when (not (float= slope (aref new-slopes (1- (length new-slopes))) +weight-eps+))
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
       (let* ((seg-0 (bisect-left breakpoints 0d0 :order #'key<))
              (value intercept)
              (current-x 0d0))
         (cond
           ((>= x 0d0)
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
  (when (float= weight 0d0 +weight-eps+)
    (return-from pl-add pl))
  (let* ((breakpoints (%pl-breakpoints pl))
         (slopes (%pl-slopes pl))
         (index (bisect-left breakpoints a :order #'key<)))
    (unless (and (< index (length breakpoints))
                 (float= a (aref breakpoints index) +key-eps+))
      ;; Insert breakpoint
      (vector-push-extend 0d0 breakpoints)
      (loop for i from (1- (length breakpoints)) downto (1+ index)
            do (setf (aref breakpoints i) (aref breakpoints (1- i))))
      (setf (aref breakpoints index) a)
      ;; Insert slope (copy from current interval)
      (vector-push-extend 0d0 slopes)
      (loop for i from (1- (length slopes)) downto (+ index 1)
            do (setf (aref slopes i) (aref slopes (1- i)))))
    (if (float< 0d0 weight +weight-eps+)
        ;; Increase slopes for x >= a (that's slopes[pos+1] onwards)
        (loop for i from (1+ index) below (length slopes)
              do (incf (aref slopes i) weight))
        ;; Increase slopes for x < a (that's slopes[0..pos])
        (loop for i from 0 to index
              do (incf (aref slopes i) weight)))
    ;; Update intercept: max(0, weight*(x-a)) at x=0 is max(0, -weight*a)
    (incf (%pl-intercept pl) (max 0d0 (* (- weight) a)))
    (pl-merge pl)))

(defun pl-delete (pl a weight)
  "Subtracts max(0, weight*(x-a)) from f.
For weight > 0: slope decreases by weight for x >= a.
For weight < 0: slope decreases by weight for x < a.

The behaviour is undefined if the convexity is broken."
  (when (float= weight 0d0 +weight-eps+)
    (return-from pl-delete pl))
  (let* ((breakpoints (%pl-breakpoints pl))
         (slopes (%pl-slopes pl))
         (index (bisect-left breakpoints a :order #'key<)))
    (unless (and (< index (length breakpoints))
                 (float= a (aref breakpoints index) +key-eps+))
      ;; Insert breakpoint
      (vector-push-extend 0d0 breakpoints)
      (loop for i from (1- (length breakpoints)) downto (1+ index)
            do (setf (aref breakpoints i) (aref breakpoints (1- i))))
      (setf (aref breakpoints index) a)
      ;; Insert slope (copy from current interval)
      (vector-push-extend 0d0 slopes)
      (loop for i from (1- (length slopes)) downto (1+ index)
            do (setf (aref slopes i) (aref slopes (1- i)))))
    (if (float< 0d0 weight +weight-eps+)
        ;; Decrease slopes for x >= a (that's slopes[index+1] onwards)
        (loop for i from (1+ index) below (length slopes)
              do (decf (aref slopes i) weight))
        ;; Decrease slopes for x < a (that's slopes[0..index])
        (loop for i from 0 to index
            do (decf (aref slopes i) weight)))
    ;; Update intercept: subtract max(0, -weight*a) (inverse of pl-add)
    (decf (%pl-intercept pl) (max 0d0 (* (- weight) a)))
    (pl-merge pl)))

(defun pl-max-affine (pl a b)
  "Replaces f(x) with max(f(x), ax+b). f must be convex.
h(x) = f(x) - ax - b is convex, so {x : h(x) <= 0} is a contiguous interval [x1, x2].
Outside that interval f dominates; inside, the line ax+b dominates."
  (let* ((old-bp (%pl-breakpoints pl))
         (old-slopes (%pl-slopes pl))
         (n (length old-bp))
         (nseg (length old-slopes)))
    ;; h-slope for segment k is (old-slopes[k] - a)
    ;; Evaluate h at breakpoints
    (let ((hv (make-array n :element-type 'double-float)))
      (dotimes (i n)
        (let ((x (aref old-bp i)))
          (setf (aref hv i) (- (pl-value pl x) (* a x) b))))
      ;; Quick check: if h >= 0 at all breakpoints and on infinite segments, f dominates
      (let ((min-h (- (%pl-intercept pl) b)))
        (dotimes (i n) (setq min-h (min min-h (aref hv i))))
        (when (and (>= min-h (- +key-eps+))
                   (<= (- (aref old-slopes 0) a) 0d0)        ; h non-increasing on left
                   (>= (- (aref old-slopes (1- nseg)) a) 0d0)) ; h non-decreasing on right
          (return-from pl-max-affine pl)))
      ;; Find x1 (leftmost zero of h) and x2 (rightmost zero of h)
      (let* ((h0 (- (%pl-intercept pl) b))
             (x1 (find-h-left-zero old-bp old-slopes hv n a h0))
             (x2 (find-h-right-zero old-bp old-slopes hv n a h0)))
        (when (or (null x1) (null x2))
          (return-from pl-max-affine pl))
        ;; Rebuild: f for x < x1, L for x1..x2, f for x > x2
        (let ((new-bp (make-array 0 :element-type 'double-float :adjustable t :fill-pointer 0))
              (new-slopes (make-array 0 :element-type 'double-float :adjustable t :fill-pointer 0)))
          ;; Left part (x < x1)
          (if (= x1 +negative-inf+)
              (vector-push-extend a new-slopes)
              (progn
                (vector-push-extend (aref old-slopes 0) new-slopes)
                (loop for i from 0 below n
                      while (float< (aref old-bp i) x1 +key-eps+)
                      do (vector-push-extend (aref old-bp i) new-bp)
                         (vector-push-extend (aref old-slopes (1+ i)) new-slopes))
                (vector-push-extend x1 new-bp)
                (vector-push-extend a new-slopes)))
          ;; Right part (x > x2)
          (unless (= x2 +positive-inf+)
            (vector-push-extend x2 new-bp)
            (let ((seg (bisect-left old-bp x2 :order #'key<)))
              (when (and (< seg n) (float= x2 (aref old-bp seg) +key-eps+))
                (incf seg))
              (vector-push-extend (aref old-slopes seg) new-slopes)
              (loop for i from seg below n
                    do (vector-push-extend (aref old-bp i) new-bp)
                       (vector-push-extend (aref old-slopes (1+ i)) new-slopes))))
          (setf (%pl-breakpoints pl) new-bp
                (%pl-slopes pl) new-slopes
                (%pl-intercept pl) (max (%pl-intercept pl) b)))
        (pl-merge pl)))))

(defun find-h-left-zero (bp slopes hv n a &optional (h0 0d0))
  "Find x1: the leftmost x where h(x)=f(x)-ax-b transitions from positive to zero.
Returns +negative-inf+ if h <= 0 extends to -infinity, NIL if h >= 0 everywhere.
H0 is h(0) = intercept - b, used when n=0."
  (let ((hs0 (- (aref slopes 0) a)))
    (cond
      ;; n=0: single segment, h(x) = h0 + hs0*x, zero at x*=-h0/hs0
      ;; hs0>0: h increasing, {h<=0}=(-inf,x*]. x1=-inf
      ;; hs0<0: h decreasing, {h<=0}=[x*,+inf). x1=x*
      ;; hs0=0: h constant. <=0 => x1=-inf, >0 => nil
      ((zerop n)
       (cond
         ((> hs0 0d0) +negative-inf+)
         ((< hs0 0d0) (- (/ h0 hs0)))
         (t (if (<= h0 0d0) +negative-inf+ nil))))
      ;; h -> -inf as x -> -inf => x1 = -inf
      ((> hs0 0d0) +negative-inf+)
      ;; h constant on left
      ((= hs0 0d0)
       (if (<= (aref hv 0) 0d0)
           +negative-inf+
           (scan-left-zero-internal bp slopes hv n a)))
      ;; h -> +inf as x -> -inf, decreasing on left
      (t
       (if (<= (aref hv 0) 0d0)
           ;; Zero in left segment: h(x) = hv[0] + hs0*(x - bp[0])
           (- (aref bp 0) (/ (aref hv 0) hs0))
           (scan-left-zero-internal bp slopes hv n a))))))

(defun scan-left-zero-internal (bp slopes hv n a)
  "Scan internal and right segments to find x1."
  ;; Internal segments
  (loop for i from 0 below (1- n)
        when (and (> (aref hv i) 0d0) (<= (aref hv (1+ i)) 0d0))
        do (let ((hs (- (aref slopes (1+ i)) a)))
             (return-from scan-left-zero-internal
               (if (/= hs 0d0)
                   (- (aref bp i) (/ (aref hv i) hs))
                   (aref bp (1+ i))))))
  ;; Right segment
  (when (> n 0)
    (let ((hn (aref hv (1- n)))
          (hsn (- (aref slopes n) a)))
      (when (and (> hn 0d0) (< hsn 0d0))
        (- (aref bp (1- n)) (/ hn hsn))))))

(defun find-h-right-zero (bp slopes hv n a &optional (h0 0d0))
  "Find x2: the rightmost x where h(x)=f(x)-ax-b transitions from zero to positive.
Returns +positive-inf+ if h <= 0 extends to +infinity, NIL if h >= 0 everywhere.
H0 is h(0) = intercept - b, used when n=0."
  (let ((hsn (- (aref slopes n) a)))
    (cond
      ;; n=0: single segment, h(x) = h0 + hsn*x, zero at x*=-h0/hsn
      ;; hsn>0: h increasing, {h<=0}=(-inf,x*]. x2=x*
      ;; hsn<0: h decreasing, {h<=0}=[x*,+inf). x2=+inf
      ;; hsn=0: h constant. <=0 => x2=+inf, >0 => nil
      ((zerop n)
       (cond
         ((< hsn 0d0) +positive-inf+)
         ((> hsn 0d0) (- (/ h0 hsn)))
         (t (if (<= h0 0d0) +positive-inf+ nil))))
      ;; h -> -inf as x -> +inf => x2 = +inf
      ((< hsn 0d0) +positive-inf+)
      ;; h constant on right
      ((= hsn 0d0)
       (if (<= (aref hv (1- n)) 0d0)
           +positive-inf+
           (scan-right-zero-internal bp slopes hv n a)))
      ;; h -> +inf as x -> +inf, increasing on right
      (t
       (if (<= (aref hv (1- n)) 0d0)
           ;; Zero in right segment: h(x) = hv[n-1] + hsn*(x - bp[n-1])
           (- (aref bp (1- n)) (/ (aref hv (1- n)) hsn))
           (scan-right-zero-internal bp slopes hv n a))))))

(defun scan-right-zero-internal (bp slopes hv n a)
  "Scan internal and left segments from right to find x2."
  ;; Internal segments (scan right to left)
  (loop for i from (- n 2) downto 0
        when (and (<= (aref hv i) 0d0) (> (aref hv (1+ i)) 0d0))
        do (let ((hs (- (aref slopes (1+ i)) a)))
             (return-from scan-right-zero-internal
               (if (/= hs 0d0)
                   ;; h(x) = hv[i] + hs*(x - bp[i]), solve h=0
                   (- (aref bp i) (/ (aref hv i) hs))
                   (aref bp i)))))
  ;; Left segment: h goes from {-inf if hs0>0, +inf if hs0<0, hv[0] if hs0=0} to hv[0]
  ;; x2 is in this segment if the zero is here (h transitions from <=0 to >0)
  (when (> n 0)
    (let ((h0-val (aref hv 0))
          (hs0 (- (aref slopes 0) a)))
      (cond
        ;; hs0 > 0: h goes from -inf to hv[0]. Zero always exists at bp[0]-hv[0]/hs0
        ((> hs0 0d0) (- (aref bp 0) (/ h0-val hs0)))
        ;; hs0 < 0: h goes from +inf to hv[0]. If hv[0] <= 0, zero in this segment
        ((and (< hs0 0d0) (<= h0-val 0d0))
         (- (aref bp 0) (/ h0-val hs0)))
        ;; hs0 = 0: h constant = hv[0]. No transition.
        (t nil)))))

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
          (multiple-value-bind (left-slope right-slope) (pl-subdiff pl 0d0)
            (declare (ignore left-slope))
            (if (float<= right-slope c +weight-eps+)
                ;; Slope at 0 is <= C, so minimum is at t = 0
                (%pl-intercept pl)
                ;; Find point where slope = C (it must be < 0)
                (multiple-value-bind (left right) (pl-arg-subdiff pl c)
                  (declare (ignore left))
                  (if (and (< +negative-inf+ right +positive-inf+)
                           (float<= right 0d0 +key-eps+))
                      (- (pl-value pl right) (* c right))
                      ;; Fallback: minimum at t = 0
                      (%pl-intercept pl)))))))
    (let ((breakpoints (%pl-breakpoints pl))
          (slopes (%pl-slopes pl)))
      (cond
        ;; If leftmost slope > c, make constant c
        ((float< c (aref slopes 0) +weight-eps+)
         (setf (fill-pointer breakpoints) 0)
         (setf (fill-pointer slopes) 1)
         (setf (aref slopes 0) c))
        (t
         ;; Find first breakpoint where slope becomes > c
         (let ((cut-index nil))
           (dotimes (i (length breakpoints))
             (when (float< c (aref slopes (1+ i)) +weight-eps+)
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
          (multiple-value-bind (left-slope right-slope) (pl-subdiff pl 0d0)
            (declare (ignore right-slope))
            (if (float<= c left-slope +weight-eps+)
                ;; Slope at 0 is >= C, so minimum is at t = 0
                (%pl-intercept pl)
                ;; Find point where slope = C (it must be > 0)
                (multiple-value-bind (left right) (pl-arg-subdiff pl c)
                  (declare (ignore right))
                  (if (and (< +negative-inf+ left +positive-inf+)
                           (float<= 0d0 left +key-eps+))
                      (- (pl-value pl left) (* c left))
                      ;; Fallback: minimum at t = 0
                      (%pl-intercept pl)))))))
    (let ((breakpoints (%pl-breakpoints pl))
          (slopes (%pl-slopes pl)))
      (cond
        ;; If rightmost slope < c, make constant c
        ((float< (aref slopes (1- (length slopes))) c +weight-eps+)
         (setf (fill-pointer breakpoints) 0)
         (setf (fill-pointer slopes) 1)
         (setf (aref slopes 0) c))
        (t
         ;; Find last breakpoint where slope BEFORE it is < c
         (let ((cut-index nil))
           (loop for i from (1- (length breakpoints)) downto 0
                 when (float< (aref slopes i) c +weight-eps+)
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
                      ((float<= slope-right-l 0d0 +weight-eps+)
                       (pl-value pl right-bound))
                      ;; Minimum at left bound (slope >= 0 throughout interval)
                      ((float<= 0d0 slope-left-r +weight-eps+)
                       (pl-value pl left-bound))
                      ;; Minimum where slope = 0
                      (t
                       (multiple-value-bind (left right) (pl-arg-subdiff pl 0d0)
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
         (let ((new-bp (make-array 0 :element-type 'double-float :adjustable t :fill-pointer 0))
               (new-slopes (make-array 0 :element-type 'double-float :adjustable t :fill-pointer 0)))
           (vector-push-extend (aref slopes 0) new-slopes)
           (dotimes (i (length breakpoints))
             (let ((left-slope (aref slopes i))
                   (right-slope (aref slopes (1+ i)))
                   (bp (aref breakpoints i)))
               (cond
                 ;; Only left: slope before is negative, slope after is non-positive
                 ((and (float< left-slope 0d0 +weight-eps+)
                       (float<= right-slope 0d0 +weight-eps+))
                  (vector-push-extend (+ bp ldelta) new-bp)
                  (vector-push-extend right-slope new-slopes))
                 ;; Only right: slope before is non-negative, slope after is positive
                 ((and (float<= 0d0 left-slope +weight-eps+)
                       (float< 0d0 right-slope +weight-eps+))
                  (vector-push-extend (+ bp rdelta) new-bp)
                  (vector-push-extend right-slope new-slopes))
                 ;; Both: slope before is negative, slope after is positive
                 ;; Split into two breakpoints with a flat region between
                 ((and (float< left-slope 0d0 +weight-eps+)
                       (float< 0d0 right-slope +weight-eps+))
                  (vector-push-extend (+ bp ldelta) new-bp)
                  (vector-push-extend 0d0 new-slopes)
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
      (cond ((float< diff base-slope +weight-eps+) (values +negative-inf+ +negative-inf+))
            ((float< end-slope diff +weight-eps+) (values +positive-inf+ +positive-inf+))
            (t
             (let ((left-end (if (float= diff base-slope +weight-eps+)
                                  +negative-inf+
                                  nil))
                   (right-end (if (float= diff end-slope +weight-eps+)
                                   +positive-inf+
                                   nil)))
               ;; Find left-end: first breakpoint where slope becomes >= diff
               (unless left-end
                 (loop for i from 0 below (length breakpoints)
                       when (and (float< (aref slopes i) diff +weight-eps+)
                                 (float<= diff (aref slopes (1+ i)) +weight-eps+))
                       do (setq left-end (aref breakpoints i))
                          (return)))
               ;; Find right-end: last breakpoint where slope becomes > diff
               (unless right-end
                 (loop for i from (1- (length breakpoints)) downto 0
                       when (and (float<= (aref slopes i) diff +weight-eps+)
                                 (float< diff (aref slopes (1+ i)) +weight-eps+))
                       do (setq right-end (aref breakpoints i))
                          (return)))
               (values left-end right-end)))))))

(defun pl-subdiff (pl x)
  "Returns the subdifferential at x as (values left-slope right-slope)."
  (let ((breakpoints (%pl-breakpoints pl))
        (slopes (%pl-slopes pl)))
    ;; Find the interval containing x
    (let ((index (bisect-left breakpoints x :order #'key<)))
      (if (and (< index (length breakpoints))
               (float= x (aref breakpoints index) +key-eps+))
          (values (aref slopes index) (aref slopes (1+ index)))
          (values (aref slopes index) (aref slopes index))))))

(test slope-trick-operation-rational/random
  (let ((*random-state* (sb-ext:seed-random-state 0))
        (*test-dribble* nil))
    (dotimes (_ 10000)
      (let* ((denom (float (1+ (random 20)) 0d0))
             (base-slope (/ (float (- (random 10) 5) 0d0) denom))
             (mstrick (make-multi-slope-trick base-slope))
             (pl (make-pl))
             (add-history nil))
        (pl-add-linear pl base-slope)
        (dotimes (i 100)
          (ecase (random 16)
            ((0 1 2 3 4 5)
             ;; add
             (let ((a (/ (float (- (random 20) 10) 0d0) denom))
                   (weight (/ (float (- (random 20) 10) 0d0) denom)))
               (mstrick-add mstrick a weight)
               (pl-add pl a weight)
               (push (cons a weight) add-history)))
            ((6)
             ;; add-linear
             (let ((slope (/ (float (- (random 20) 10) 0d0) denom)))
               (mstrick-add-linear mstrick slope)
               (pl-add-linear pl slope)))
            ((7)
             ;; shift operation
             (let* ((ldelta (/ (float (- (random 20) 15) 0d0) denom))
                    (rdelta (+ ldelta (/ (float (random 10) 0d0) denom))))
               (mstrick-shift mstrick ldelta rdelta)
               (pl-shift pl ldelta rdelta)
               (setq add-history nil)))
            ((8 9)
             (let ((rollback-p (zerop (random 3))))
               (if (zerop (random 2))
                   (let* ((c (+ (%mstrick-base-slope mstrick)
                                (/ (float (random 15) 0d0) denom)))
                          (rest-part (mstrick-left-cum mstrick c)))
                     (if rollback-p
                         (mstrick-left-cum-rollback mstrick rest-part)
                         (progn
                           (pl-left-cum pl c)
                           (setq add-history nil))))
                   (let* ((slopes (%pl-slopes pl))
                          (c (- (aref slopes (- (length slopes) 1))
                                (/ (float (random 15) 0d0) denom)))
                          (rest-part (mstrick-right-cum mstrick c)))
                     (if rollback-p
                         (mstrick-right-cum-rollback mstrick rest-part)
                         (progn
                           (pl-right-cum pl c)
                           (setq add-history nil)))))))
            ((10 11)
             ;; check function value
             (let ((xs (subseq (shuffle! (coerce (loop for xi from -12 to 12
                                                       collect (/ (float xi 0d0) denom))
                                                 'vector))
                               0 (+ 1 (random 15)))))
               (is-true
                (loop for x across xs
                      always (approx= (mstrick-value mstrick x)
                                       (pl-value pl x))))))
            ((12 13)
             ;; check subdiff and arg-subdiff
             (let ((xs (subseq (shuffle! (coerce (loop for xi from -21 to 21
                                                       collect (/ (float xi 0d0) denom))
                                                 'vector))
                               0 10)))
               (is-true
                (loop for x across xs
                      always (multiple-value-bind (ms-l ms-r) (mstrick-subdiff mstrick x)
                               (multiple-value-bind (pl-l pl-r) (pl-subdiff pl x)
                                 (and (approx= ms-l pl-l)
                                      (approx= ms-r pl-r)))))))
             (let ((diffs (subseq (shuffle! (coerce (loop for xi from -21 to 21
                                                          collect (/ (float xi 0d0) denom))
                                                    'vector))
                                  0 10)))
               (is-true
                (loop for diff across diffs
                      always (multiple-value-bind (ms-l ms-r) (mstrick-arg-subdiff mstrick diff)
                               (multiple-value-bind (pl-l pl-r) (pl-arg-subdiff pl diff)
                                 (and (approx= ms-l pl-l)
                                      (approx= ms-r pl-r))))))))
            ((14 15)
             ;; delete (rollback a random add from history)
             (when add-history
               (let* ((idx (random (length add-history)))
                      (entry (nth idx add-history))
                      (a (car entry))
                      (weight (cdr entry)))
                 (mstrick-delete mstrick a weight)
                 (pl-delete pl a weight)
                 (setq add-history
                       (nconc (subseq add-history 0 idx)
                              (nthcdr (+ idx 1) add-history))))))))))))

(test slope-trick-operation-float/random
  (let ((*random-state* (sb-ext:seed-random-state 42))
        (*test-dribble* nil))
    (dotimes (_ 10000)
      (let* ((base-slope (- (random 10d0) 5d0))
             (mstrick (make-multi-slope-trick base-slope))
             (pl (make-pl))
             (add-history nil))
        (pl-add-linear pl base-slope)
        (dotimes (i 100)
          (ecase (random 16)
            ((0 1 2 3 4 5)
             ;; add
             (let ((a (- (random 20d0) 10d0))
                   (weight (- (random 20d0) 10d0)))
               (mstrick-add mstrick a weight)
               (pl-add pl a weight)
               (push (cons a weight) add-history)))
            ((6)
             ;; add-linear
             (let ((slope (- (random 20d0) 10d0)))
               (mstrick-add-linear mstrick slope)
               (pl-add-linear pl slope)))
            ((7)
             ;; shift operation
             (let* ((ldelta (- (random 20d0) 15d0))
                    (rdelta (+ ldelta (random 10d0))))
               (mstrick-shift mstrick ldelta rdelta)
               (pl-shift pl ldelta rdelta)
               (setq add-history nil)))
            ((8 9)
             (let ((rollback-p (zerop (random 3))))
               (if (zerop (random 2))
                   (let* ((c (+ (%mstrick-base-slope mstrick) (random 15d0)))
                          (rest-part (mstrick-left-cum mstrick c)))
                     (if rollback-p
                         (mstrick-left-cum-rollback mstrick rest-part)
                         (progn
                           (pl-left-cum pl c)
                           (setq add-history nil))))
                   (let* ((slopes (%pl-slopes pl))
                          (c (- (aref slopes (- (length slopes) 1))
                                (random 15d0)))
                          (rest-part (mstrick-right-cum mstrick c)))
                     (if rollback-p
                         (mstrick-right-cum-rollback mstrick rest-part)
                         (progn
                           (pl-right-cum pl c)
                           (setq add-history nil)))))))
            ((10 11)
             ;; check function value
             (let ((xs (coerce (loop repeat (+ 1 (random 15))
                                     collect (- (random 24d0) 12d0))
                               'vector)))
               (is-true
                (loop for x across xs
                      always (approx= (mstrick-value mstrick x)
                                      (pl-value pl x))))))
            ((12 13)
             ;; check subdiff and arg-subdiff
             (let ((xs (coerce (loop repeat 10
                                     collect (- (random 42d0) 21d0))
                               'vector)))
               (is-true
                (loop for x across xs
                      always (multiple-value-bind (ms-l ms-r) (mstrick-subdiff mstrick x)
                               (multiple-value-bind (pl-l pl-r) (pl-subdiff pl x)
                                 (and (approx= ms-l pl-l)
                                      (approx= ms-r pl-r)))))))
             (let ((diffs (coerce (loop repeat 10
                                        collect (- (random 42d0) 21d0))
                                  'vector)))
               (is-true
                (loop for diff across diffs
                      always (multiple-value-bind (ms-l ms-r) (mstrick-arg-subdiff mstrick diff)
                               (multiple-value-bind (pl-l pl-r) (pl-arg-subdiff pl diff)
                                 (and (approx= ms-l pl-l)
                                      (approx= ms-r pl-r))))))))
            ((14 15)
             ;; delete (rollback a random add from history)
             (when add-history
               (let* ((idx (random (length add-history)))
                      (entry (nth idx add-history))
                      (a (car entry))
                      (weight (cdr entry)))
                 (mstrick-delete mstrick a weight)
                 (pl-delete pl a weight)
                 (setq add-history
                       (nconc (subseq add-history 0 idx)
                              (nthcdr (+ idx 1) add-history))))))))))))

(defun test-hand ()
  (let ((*random-state* (sb-ext:seed-random-state 2)))
    (let ((ms (make-multi-slope-trick -5d0)))
      (print ms)
      (mstrick-add ms 4d0 -1d0)
      (print ms)
      (mstrick-add ms 3d0 1d0)
      (print ms)
      (mstrick-shift ms -3d0 -3d0)
      (mstrick-value ms 10d0))))
