(defpackage :cp/test/multi-slope-trick-float
  (:use :cl :fiveam :cp/multi-slope-trick-float :cp/bisect :cp/shuffle)
  (:import-from :cp/multi-slope-trick-float
                #:%mstrick-base-slope #:%mstrick-base-value #:%mstrick-mset
                #:mset-insert
                #:float< #:float= #:float<=)
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
BASE-VALUE is the value of the function at x=0."
  (breakpoints (make-array 0 :element-type 'double-float :adjustable t :fill-pointer 0)
   :type (vector double-float))
  (slopes (make-array 1 :element-type 'double-float :initial-element 0d0 :adjustable t :fill-pointer 1)
   :type (vector double-float))
  (base-value 0d0 :type double-float))

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
        (base-value (%pl-base-value pl)))
    (cond
      ((zerop (length breakpoints))
       ;; No breakpoints, linear function
       (+ base-value (* (aref slopes 0) x)))
      (t
       ;; Find which segment x=0 is in and integrate to x
       (let* ((seg-0 (bisect-left breakpoints 0d0 :order #'key<))
              (value base-value)
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
    ;; Update base-value: max(0, weight*(x-a)) at x=0 is max(0, -weight*a)
    (incf (%pl-base-value pl) (max 0d0 (* (- weight) a)))
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
    ;; Update base-value: subtract max(0, -weight*a) (inverse of pl-add)
    (decf (%pl-base-value pl) (max 0d0 (* (- weight) a)))
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
      (let ((min-h (- (%pl-base-value pl) b)))
        (dotimes (i n) (setq min-h (min min-h (aref hv i))))
        (when (and (>= min-h (- +key-eps+))
                   (<= (- (aref old-slopes 0) a) 0d0)        ; h non-increasing on left
                   (>= (- (aref old-slopes (1- nseg)) a) 0d0)) ; h non-decreasing on right
          (return-from pl-max-affine pl)))
      ;; Find x1 (leftmost zero of h) and x2 (rightmost zero of h)
      (let* ((h0 (- (%pl-base-value pl) b))
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
                (%pl-base-value pl) (max (%pl-base-value pl) b)))
        (pl-merge pl)))))

(defun pl-convex-hull-with-point (pl a b)
  "Replace f with the function whose epigraph is the closure of
conv(epi(f) ∪ {(a, b)})."
  (let ((fa (pl-value pl a)))
    ;; If point is already in epigraph, no change
    (when (float<= fa b +value-eps+)
      (return-from pl-convex-hull-with-point pl))
    (let* ((breakpoints (%pl-breakpoints pl))
           (slopes (%pl-slopes pl))
           (n (length breakpoints))
           (nseg (length slopes)))
      ;; Compute V[i] for each segment: value at x=a of the tangent line with slope s[i].
      ;; V is concave in the slope index and reaches its maximum f(a) at the segment
      ;; containing a. We use V to find tangent points from (a, b) to f.
      (let ((v (make-array nseg :element-type 'double-float)))
        (cond
          ((zerop n)
           (setf (aref v 0) (+ (%pl-base-value pl) (* (aref slopes 0) a))))
          (t
           ;; V[0]: use bp[0] as reference
           (setf (aref v 0) (+ (pl-value pl (aref breakpoints 0))
                                (* (aref slopes 0) (- a (aref breakpoints 0)))))
           ;; V[i] for 1 <= i <= n: use bp[i-1] as reference
           (loop for i from 1 below nseg
                 do (setf (aref v i)
                          (+ (pl-value pl (aref breakpoints (min (1- i) (1- n))))
                             (* (aref slopes i)
                                (- a (aref breakpoints (min (1- i) (1- n))))))))))
        ;; Find left tangent: scan from i=0 upward, find first i where V[i] >= b
        (let (s-l x-l)
          (loop for i from 0 below nseg
                when (float<= b (aref v i) +value-eps+)
                do (cond
                     ((= i 0)
                      (setq s-l (aref slopes 0)
                            x-l +negative-inf+))
                     ((float= (aref v i) b +value-eps+)
                      (setq s-l (aref slopes i)
                            x-l (aref breakpoints (1- i))))
                     (t
                      (let ((bpi (aref breakpoints (1- i))))
                        (setq s-l (/ (- (pl-value pl bpi) b) (- bpi a))
                              x-l bpi))))
                   (return))
          ;; Find right tangent: scan from i=nseg-1 downward, find first i where V[i] >= b
          (let (s-r x-r)
            (loop for i from (1- nseg) downto 0
                  when (float<= b (aref v i) +value-eps+)
                  do (cond
                       ((= i n)
                        (setq s-r (aref slopes n)
                              x-r +positive-inf+))
                       ((float= (aref v i) b +value-eps+)
                        (setq s-r (aref slopes i)
                              x-r (aref breakpoints i)))
                       (t
                        (let ((bpi (aref breakpoints i)))
                          (setq s-r (/ (- (pl-value pl bpi) b) (- bpi a))
                                x-r bpi))))
                     (return))
            ;; Rebuild the function
            (let ((new-bp (make-array 0 :element-type 'double-float :adjustable t :fill-pointer 0))
                  (new-slopes (make-array 0 :element-type 'double-float :adjustable t :fill-pointer 0)))
              ;; Left part (x < x-l)
              (if (= x-l +negative-inf+)
                  (vector-push-extend s-l new-slopes)
                  (progn
                    (vector-push-extend (aref slopes 0) new-slopes)
                    (loop for i from 0 below n
                          while (float< (aref breakpoints i) x-l +key-eps+)
                          do (vector-push-extend (aref breakpoints i) new-bp)
                             (vector-push-extend (aref slopes (1+ i)) new-slopes))
                    (vector-push-extend x-l new-bp)
                    (vector-push-extend s-l new-slopes)))
              ;; Breakpoint at a (if s-l != s-r)
              (unless (float= s-l s-r +weight-eps+)
                (vector-push-extend a new-bp)
                (vector-push-extend s-r new-slopes))
              ;; Right part (x > x-r)
              (unless (= x-r +positive-inf+)
                (vector-push-extend x-r new-bp)
                (let ((seg (bisect-left breakpoints x-r :order #'key<)))
                  (when (and (< seg n) (float= x-r (aref breakpoints seg) +key-eps+))
                    (incf seg))
                  (vector-push-extend (aref slopes seg) new-slopes)
                  (loop for i from seg below n
                        do (vector-push-extend (aref breakpoints i) new-bp)
                           (vector-push-extend (aref slopes (1+ i)) new-slopes))))
              ;; Compute new base-value: g(0)
              (let ((new-base-value
                      (cond
                        ;; 0 in left part (unchanged)
                        ((and (/= x-l +negative-inf+) (float<= 0d0 x-l +key-eps+))
                         (%pl-base-value pl))
                        ;; 0 in right part (unchanged)
                        ((and (/= x-r +positive-inf+) (float<= x-r 0d0 +key-eps+))
                         (%pl-base-value pl))
                        ;; 0 in middle, left of a
                        ((float<= 0d0 a +key-eps+)
                         (- b (* s-l a)))
                        ;; 0 in middle, right of a
                        (t
                         (- b (* s-r a))))))
                (setf (%pl-breakpoints pl) new-bp
                      (%pl-slopes pl) new-slopes
                      (%pl-base-value pl) new-base-value)))))))
    (pl-merge pl)))

(defun find-h-left-zero (bp slopes hv n a &optional (h0 0d0))
  "Find x1: the leftmost x where h(x)=f(x)-ax-b transitions from positive to zero.
Returns +negative-inf+ if h <= 0 extends to -infinity, NIL if h >= 0 everywhere.
H0 is h(0) = base-value - b, used when n=0."
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
H0 is h(0) = base-value - b, used when n=0."
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
  (let ((new-base-value
          (multiple-value-bind (left-slope right-slope) (pl-subdiff pl 0d0)
            (declare (ignore left-slope))
            (if (float<= right-slope c +weight-eps+)
                ;; Slope at 0 is <= C, so minimum is at t = 0
                (%pl-base-value pl)
                ;; Find point where slope = C (it must be < 0)
                (multiple-value-bind (left right) (pl-arg-subdiff pl c)
                  (declare (ignore left))
                  (if (and (< +negative-inf+ right +positive-inf+)
                           (float<= right 0d0 +key-eps+))
                      (- (pl-value pl right) (* c right))
                      ;; Fallback: minimum at t = 0
                      (%pl-base-value pl)))))))
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
    (setf (%pl-base-value pl) new-base-value))
  (pl-merge pl))

(defun pl-right-cum (pl c)
  "g(x) = min_{x <= t} (f(t) - Ct) + Cx.
Clips slopes to [C, infinity)."
  ;; g(0) = min_{t >= 0} (f(t) - Ct)
  ;; For convex f, minimum of f(t) - Ct is where slope_f = C
  ;; If slope at 0 >= C, then minimum for t >= 0 is at t = 0
  ;; Otherwise, find point t* >= 0 where slope = C
  (let ((new-base-value
          (multiple-value-bind (left-slope right-slope) (pl-subdiff pl 0d0)
            (declare (ignore right-slope))
            (if (float<= c left-slope +weight-eps+)
                ;; Slope at 0 is >= C, so minimum is at t = 0
                (%pl-base-value pl)
                ;; Find point where slope = C (it must be > 0)
                (multiple-value-bind (left right) (pl-arg-subdiff pl c)
                  (declare (ignore right))
                  (if (and (< +negative-inf+ left +positive-inf+)
                           (float<= 0d0 left +key-eps+))
                      (- (pl-value pl left) (* c left))
                      ;; Fallback: minimum at t = 0
                      (%pl-base-value pl)))))))
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
    (setf (%pl-base-value pl) new-base-value))
  (pl-merge pl))

(defun pl-shift (pl ldelta &optional rdelta)
  "g(x) = min_{x-rdelta <= t <= x-ldelta} f(t).
Shifts left breakpoints (negative slope before) by ldelta,
shifts right breakpoints (positive slope after) by rdelta."
  (let ((rdelta (or rdelta ldelta)))
    (assert (<= ldelta rdelta))
    ;; Compute new base-value: g(0) = min_{-rdelta <= t <= -ldelta} f(t)
    ;; For convex f, minimum over [a, b] is at:
    ;; - a if slope at a >= 0
    ;; - b if slope at b <= 0
    ;; - point where slope = 0 if it's in [a, b]
    (let* ((left-bound (- rdelta))
           (right-bound (- ldelta)))
      (if (= left-bound right-bound)
          ;; Uniform shift: g(0) = f(-delta)
          (setf (%pl-base-value pl) (pl-value pl left-bound))
          ;; Find minimum over interval
          (multiple-value-bind (slope-left-l slope-left-r) (pl-subdiff pl left-bound)
            (declare (ignore slope-left-l))
            (multiple-value-bind (slope-right-l slope-right-r) (pl-subdiff pl right-bound)
              (declare (ignore slope-right-r))
              (setf (%pl-base-value pl)
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

(defun make-test-pl (breakpoints slopes base-value)
  "Create a piecewise-linear function from explicit breakpoints, slopes, and base-value."
  (let ((pl (make-pl)))
    (setf (%pl-breakpoints pl)
          (make-array (length breakpoints) :element-type 'double-float
                      :initial-contents breakpoints :adjustable t
                      :fill-pointer (length breakpoints)))
    (setf (%pl-slopes pl)
          (make-array (length slopes) :element-type 'double-float
                      :initial-contents slopes :adjustable t
                      :fill-pointer (length slopes)))
    (setf (%pl-base-value pl) (coerce base-value 'double-float))
    pl))

(defun make-test-mstrick (breakpoints slopes base-value)
  "Create a multi-slope-trick matching a piecewise-linear specification."
  (let* ((base-slope (coerce (first slopes) 'double-float))
         (ms (make-multi-slope-trick base-slope)))
    (when breakpoints
      (setf (%mstrick-base-value ms)
            (pl-value (make-test-pl (mapcar (lambda (x) (coerce x 'double-float)) breakpoints)
                                    (mapcar (lambda (x) (coerce x 'double-float)) slopes)
                                    (coerce base-value 'double-float))
                      (coerce (first breakpoints) 'double-float)))
      (loop for i from 0 below (length breakpoints)
            for bp in breakpoints
            for weight = (- (nth (1+ i) slopes) (nth i slopes))
            do (setf (%mstrick-mset ms)
                     (mset-insert (%mstrick-mset ms)
                                  (coerce bp 'double-float)
                                  (coerce weight 'double-float)))))
    (unless breakpoints
      (setf (%mstrick-base-value ms) (coerce base-value 'double-float)))
    ms))

(test convex-hull-with-point/handmade
  ;; Case 1: Point in epigraph (no-op). f(x) = |x|, point (1, 2). f(1)=1 <= 2.
  (let ((pl (make-test-pl '(0d0) '(-1d0 1d0) 0d0))
        (ms (make-test-mstrick '(0) '(-1 1) 0)))
    (pl-convex-hull-with-point pl 1d0 2d0)
    (mstrick-convex-hull-with-point ms 1d0 2d0)
    (is (approx= (pl-value pl 0d0) 0d0))
    (is (approx= (mstrick-value ms 0d0) 0d0))
    (is (approx= (mstrick-value ms 1d0) 1d0))
    (is (approx= (mstrick-value ms -1d0) 1d0)))

  ;; Case 2: Point exactly on f (no-op). f(x) = |x|, point (1, 1).
  (let ((pl (make-test-pl '(0d0) '(-1d0 1d0) 0d0))
        (ms (make-test-mstrick '(0) '(-1 1) 0)))
    (pl-convex-hull-with-point pl 1d0 1d0)
    (mstrick-convex-hull-with-point ms 1d0 1d0)
    (is (approx= (mstrick-value ms 0d0) 0d0))
    (is (approx= (mstrick-value ms 1d0) 1d0)))

  ;; Case 3: f is a single line f(x)=2x+3, point (1, 2). f(1)=5>2.
  ;; g is the parallel line through (1,2): g(x) = 2x. g(0)=0.
  (let ((pl (make-test-pl '() '(2d0) 3d0))
        (ms (make-test-mstrick '() '(2) 3)))
    (pl-convex-hull-with-point pl 1d0 2d0)
    (mstrick-convex-hull-with-point ms 1d0 2d0)
    (is (approx= (mstrick-value ms 0d0) 0d0))
    (is (approx= (mstrick-value ms 1d0) 2d0))
    (is (approx= (mstrick-value ms -1d0) -2d0)))

  ;; Case 4: f(x) = |x|, point (0, -1) directly below minimum.
  ;; Both tangents at infinity. g(x) = |x| - 1.
  (let ((pl (make-test-pl '(0d0) '(-1d0 1d0) 0d0))
        (ms (make-test-mstrick '(0) '(-1 1) 0)))
    (pl-convex-hull-with-point pl 0d0 -1d0)
    (mstrick-convex-hull-with-point ms 0d0 -1d0)
    (is (approx= (mstrick-value ms 0d0) -1d0))
    (is (approx= (mstrick-value ms 1d0) 0d0))
    (is (approx= (mstrick-value ms -1d0) 0d0))
    (is (approx= (mstrick-value ms 5d0) 4d0)))

  ;; Case 5: f(x) = |x|, point (2, -1) to the right of minimum.
  ;; Left tangent from (2,-1) to f at x=0, slope = (0-(-1))/(0-2) = -0.5.
  ;; Right tangent slope = 1 (f's rightmost slope), x_R = +inf.
  ;; g: -x (x<0), -0.5x (0<=x<=2), x-3 (x>=2).
  (let ((pl (make-test-pl '(0d0) '(-1d0 1d0) 0d0))
        (ms (make-test-mstrick '(0) '(-1 1) 0)))
    (pl-convex-hull-with-point pl 2d0 -1d0)
    (mstrick-convex-hull-with-point ms 2d0 -1d0)
    (is (approx= (mstrick-value ms -2d0) 2d0))
    (is (approx= (mstrick-value ms 0d0) 0d0))
    (is (approx= (mstrick-value ms 1d0) -0.5d0))
    (is (approx= (mstrick-value ms 2d0) -1d0))
    (is (approx= (mstrick-value ms 5d0) 2d0)))

  ;; Case 6: f(x) = max(0, x), point (-1, -1).
  ;; Left tangent slope = 0 (f's leftmost), x_L = -inf.
  ;; Right tangent from (-1,-1) with slope 1 gives y=x, which coincides with f for x>=0.
  ;; g: -1 (x<=-1), x (x>=-1).
  (let ((pl (make-test-pl '(0d0) '(0d0 1d0) 0d0))
        (ms (make-test-mstrick '(0) '(0 1) 0)))
    (pl-convex-hull-with-point pl -1d0 -1d0)
    (mstrick-convex-hull-with-point ms -1d0 -1d0)
    (is (approx= (mstrick-value ms -3d0) -1d0))
    (is (approx= (mstrick-value ms -1d0) -1d0))
    (is (approx= (mstrick-value ms 0d0) 0d0))
    (is (approx= (mstrick-value ms 3d0) 3d0)))

  ;; Case 7: Right tangent coincides with f on an interval.
  ;; f: bp=[0,2,4], slopes=[-1,-0.5,0.5,1], base-value=0.
  ;; f(0)=0, f(2)=-1, f(4)=0.
  ;; Point (1, -1.5). f(1)=-0.5 > -1.5.
  ;; Left tangent: slope -1, x_L = -inf (V[0]=-1 >= -1.5).
  ;; Right tangent: V[2]=f(2)+0.5*(1-2)=-1.5 = b exactly. s_R=0.5, x_R=4.
  ;; g on [1,4] has slope 0.5, which equals f on [2,4]. Tangent coincides!
  ;; g: slope -1 (x<1), slope 0.5 (1<=x<=4), slope 1 (x>4).
  (let ((pl (make-test-pl '(0d0 2d0 4d0) '(-1d0 -0.5d0 0.5d0 1d0) 0d0))
        (ms (make-test-mstrick '(0 2 4) '(-1 -0.5 0.5 1) 0)))
    (pl-convex-hull-with-point pl 1d0 -1.5d0)
    (mstrick-convex-hull-with-point ms 1d0 -1.5d0)
    (is (approx= (mstrick-value ms -1d0) 0.5d0))
    (is (approx= (mstrick-value ms 0d0) -0.5d0))
    (is (approx= (mstrick-value ms 1d0) -1.5d0))
    (is (approx= (mstrick-value ms 2d0) -1d0))   ; = f(2), coincides
    (is (approx= (mstrick-value ms 4d0) 0d0))    ; = f(4), tangent point
    (is (approx= (mstrick-value ms 5d0) 1d0)))   ; = f(5)

  ;; Case 8: Left tangent coincides with f on an interval.
  ;; Same f. Point (3, -1.5). f(3)=-0.5 > -1.5.
  ;; Left tangent: V[1]=f(0)+(-0.5)*(3-0)=-1.5 = b exactly. s_L=-0.5, x_L=0.
  ;; g on [0,3] has slope -0.5, which equals f on [0,2]. Tangent coincides!
  ;; Right tangent: slope 1, x_R = +inf.
  ;; g: slope -1 (x<0), slope -0.5 (0<=x<=3), slope 1 (x>3).
  (let ((pl (make-test-pl '(0d0 2d0 4d0) '(-1d0 -0.5d0 0.5d0 1d0) 0d0))
        (ms (make-test-mstrick '(0 2 4) '(-1 -0.5 0.5 1) 0)))
    (pl-convex-hull-with-point pl 3d0 -1.5d0)
    (mstrick-convex-hull-with-point ms 3d0 -1.5d0)
    (is (approx= (mstrick-value ms -1d0) 1d0))   ; = f(-1)
    (is (approx= (mstrick-value ms 0d0) 0d0))    ; = f(0), coincides start
    (is (approx= (mstrick-value ms 2d0) -1d0))   ; = f(2), coincides end
    (is (approx= (mstrick-value ms 3d0) -1.5d0)) ; = b
    (is (approx= (mstrick-value ms 5d0) 0.5d0))) ; g(5) = -1.5 + 1*2 = 0.5

  ;; Case 9: Multiple breakpoints, tangent skips inner breakpoints.
  ;; f: bp=[-2,-1,0,1,2], slopes=[-3,-2,-1,1,2,3], base-value=0.
  ;; Symmetric: f(-2)=3, f(-1)=1, f(0)=0, f(1)=1, f(2)=3.
  ;; Point (0, -1). V[1]=-1=b, V[4]=-1=b.
  ;; Left tangent: s=-2, x_L=-2. Right tangent: s=2, x_R=2.
  ;; Inner breakpoints at -1, 0, 1 are skipped.
  ;; g: slopes [-3, -2, 2, 3], bp = [-2, 0, 2].
  (let ((pl (make-test-pl '(-2d0 -1d0 0d0 1d0 2d0) '(-3d0 -2d0 -1d0 1d0 2d0 3d0) 0d0))
        (ms (make-test-mstrick '(-2 -1 0 1 2) '(-3 -2 -1 1 2 3) 0)))
    (pl-convex-hull-with-point pl 0d0 -1d0)
    (mstrick-convex-hull-with-point ms 0d0 -1d0)
    (is (approx= (mstrick-value ms -3d0) 6d0))   ; = f(-3)
    (is (approx= (mstrick-value ms -2d0) 3d0))   ; = f(-2), tangent point
    (is (approx= (mstrick-value ms -1d0) 1d0))   ; g(-1) = -1 + (-2)*(-1) = 1
    (is (approx= (mstrick-value ms 0d0) -1d0))   ; = b
    (is (approx= (mstrick-value ms 1d0) 1d0))    ; g(1) = -1 + 2*1 = 1
    (is (approx= (mstrick-value ms 2d0) 3d0))    ; = f(2), tangent point
    (is (approx= (mstrick-value ms 3d0) 6d0)))   ; = f(3)

  ;; Case 10: Point below flat region.
  ;; f: bp=[0,4], slopes=[-1,0,1], base-value=0.
  ;; f(x) = -x (x<0), 0 (0<=x<=4), x-4 (x>4).
  ;; Point (2, -1). Tangent from (2,-1) to f at x=0 (slope -0.5) and x=4 (slope 0.5).
  ;; Flat region replaced by V shape.
  (let ((pl (make-test-pl '(0d0 4d0) '(-1d0 0d0 1d0) 0d0))
        (ms (make-test-mstrick '(0 4) '(-1 0 1) 0)))
    (pl-convex-hull-with-point pl 2d0 -1d0)
    (mstrick-convex-hull-with-point ms 2d0 -1d0)
    (is (approx= (mstrick-value ms -1d0) 1d0))   ; = f(-1)
    (is (approx= (mstrick-value ms 0d0) 0d0))    ; = f(0), tangent point
    (is (approx= (mstrick-value ms 1d0) -0.5d0))
    (is (approx= (mstrick-value ms 2d0) -1d0))   ; = b
    (is (approx= (mstrick-value ms 3d0) -0.5d0))
    (is (approx= (mstrick-value ms 4d0) 0d0))    ; = f(4), tangent point
    (is (approx= (mstrick-value ms 5d0) 1d0))))  ; = f(5)

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
          (ecase (random 20)
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
                              (nthcdr (+ idx 1) add-history))))))
            ((16 17)
             ;; max-affine
             (multiple-value-bind (x0l x0r) (pl-arg-subdiff pl 0d0)
               (declare (ignore x0r))
               (let* ((a (if (and (< +negative-inf+ x0l) (< x0l +positive-inf+))
                             x0l
                             0d0))
                      (fval (if (/= a 0d0)
                                (pl-value pl a)
                                (%pl-base-value pl)))
                      (slope (/ (float (- (random 20) 10) 0d0) denom))
                      (offset (/ (float (- (random 20) 10) 0d0) denom))
                      (line-a slope)
                      (line-b (+ fval offset (- (* slope a)))))
                 (mstrick-max-affine mstrick line-a line-b)
                 (pl-max-affine pl line-a line-b)
                 (setq add-history nil))))
            ((18 19)
             ;; convex-hull-with-point
             (multiple-value-bind (x0l x0r) (pl-arg-subdiff pl 0d0)
               (declare (ignore x0r))
               (let* ((a (if (and (< +negative-inf+ x0l) (< x0l +positive-inf+))
                             x0l
                             0d0))
                      (fval (if (/= a 0d0)
                                (pl-value pl a)
                                (%pl-base-value pl)))
                      (offset (/ (float (- (random 20) 10) 0d0) denom))
                      (point-b (+ fval offset)))
                 (mstrick-convex-hull-with-point mstrick a point-b)
                 (pl-convex-hull-with-point pl a point-b)
                 (setq add-history nil))))))))))

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
          (ecase (random 20)
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
                              (nthcdr (+ idx 1) add-history))))))
            ((16 17)
             ;; max-affine
             (multiple-value-bind (x0l x0r) (pl-arg-subdiff pl 0d0)
               (declare (ignore x0r))
               (let* ((a (if (and (< +negative-inf+ x0l) (< x0l +positive-inf+))
                             x0l
                             0d0))
                      (fval (if (/= a 0d0)
                                (pl-value pl a)
                                (%pl-base-value pl)))
                      (slope (- (random 20d0) 10d0))
                      (offset (- (random 20d0) 10d0))
                      (line-a slope)
                      (line-b (+ fval offset (- (* slope a)))))
                 (mstrick-max-affine mstrick line-a line-b)
                 (pl-max-affine pl line-a line-b)
                 (setq add-history nil))))
            ((18 19)
             ;; convex-hull-with-point
             (multiple-value-bind (x0l x0r) (pl-arg-subdiff pl 0d0)
               (declare (ignore x0r))
               (let* ((a (if (and (< +negative-inf+ x0l) (< x0l +positive-inf+))
                             x0l
                             0d0))
                      (fval (if (/= a 0d0)
                                (pl-value pl a)
                                (%pl-base-value pl)))
                      (offset (- (random 20d0) 10d0))
                      (point-b (+ fval offset)))
                 (mstrick-convex-hull-with-point mstrick a point-b)
                 (pl-convex-hull-with-point pl a point-b)
                 (setq add-history nil))))))))))

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
