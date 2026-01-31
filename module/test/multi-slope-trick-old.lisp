(defpackage :cp/test/multi-slope-trick-old
  (:use :cl :fiveam :cp/multi-slope-trick-old :cp/bisect :cp/shuffle)
  (:import-from :cp/test/base #:base-suite))
(in-package :cp/test/multi-slope-trick-old)
(in-suite base-suite)

(defstruct (piecewise-linear (:constructor make-pl ())
                             (:conc-name %pl-)
                             (:copier nil)
                             (:predicate nil))
  "A naive representation of a piecewise linear convex function.
BREAKPOINTS is a sorted vector of x-coordinates where the slope changes.
SLOPES is a vector of length (1+ n) where n is the number of breakpoints.
SLOPES[i] is the slope of the function in the interval [BREAKPOINTS[i-1], BREAKPOINTS[i])."
  (breakpoints (make-array 0 :element-type 'fixnum :adjustable t :fill-pointer 0)
   :type (vector fixnum))
  (slopes (make-array 1 :element-type 'fixnum :initial-element 0 :adjustable t :fill-pointer 1)
   :type (vector fixnum)))

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
  (pl-merge pl))

(defun pl-right-cum (pl c)
  "g(x) = min_{x <= t} (f(t) - Ct) + Cx.
Clips slopes to [C, infinity)."
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
  (pl-merge pl))

(defun pl-shift (pl ldelta &optional rdelta)
  "g(x) = min_{x-rdelta <= t <= x-ldelta} f(t).
Shifts left breakpoints (negative slope before) by ldelta,
shifts right breakpoints (positive slope after) by rdelta."
  (let ((rdelta (or rdelta ldelta)))
    (assert (<= ldelta rdelta))
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
Returns [-inf, -inf] or [+inf, +inf] if DIFF is below or above every slope of f."
  (let ((breakpoints (%pl-breakpoints pl))
        (slopes (%pl-slopes pl)))
    (let ((base-slope (aref slopes 0))
          (end-slope (aref slopes (1- (length slopes)))))
      (cond ((< diff base-slope) (values most-negative-fixnum most-negative-fixnum))
            ((< end-slope diff) (values most-positive-fixnum most-positive-fixnum))
            (t
             (let ((left-end (if (= diff base-slope)
                                 most-negative-fixnum
                                 nil))
                   (right-end (if (= diff end-slope)
                                  most-positive-fixnum
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

(test slope-trick2-operation/random
  (let ((*random-state* (sb-ext:seed-random-state 0))
        (*test-dribble* nil))
    (dotimes (_ 2000)
      (let* ((base-slope (- (random 10) 5)) 
             (mstrick (make-multi-slope-trick base-slope))
             (pl (make-pl))
             (add-history nil))
        (pl-add-linear pl base-slope)
        (dotimes (i 100)
          ;; (print mstrick)
          ;; (print pl)
          (ecase (random 17)
            ((0 1 2 3)
             ;; add
             (let ((a (- (random 20) 10))
                   (weight (- (random 20) 10)))
               ;; (format t "~%add: ~D ~D" a weight)
               (mstrick-add mstrick a weight)
               (pl-add pl a weight)
               (push (cons a weight) add-history)))
            ((4 5 6 7)
             ;; add-abs (record as two separate adds)
             (let ((a (- (random 20) 10))
                   (weight (random 10)))
               ;; (format t "~%add: ~D ~D" a weight)
               ;; (format t "~%add: ~D ~D" a (- weight))
               (mstrick-add-abs mstrick a weight)
               (pl-add-abs pl a weight)
               (push (cons a weight) add-history)
               (push (cons a (- weight)) add-history)))
            ((8)
             ;; add-linear (invalidates history since we don't track linear adds)
             (let ((slope (- (random 10) 5)))
               ;; (format t "~%add-linear: ~D" slope)
               (mstrick-add-linear mstrick slope)
               (pl-add-linear pl slope)
               (setq add-history nil)))
            ((9 10)
             ;; check subdiff and arg-subdiff with various diff values
             (let ((xs (shuffle! (coerce (loop for x from -21 to 21 collect x) 'vector))))
               (is-true (loop for x across xs
                              always (equal (multiple-value-list (mstrick-subdiff mstrick x))
                                            (multiple-value-list (pl-subdiff pl x))))))
             ;; Test arg-subdiff with multiple diff values
             (is-true (loop for diff from -20 to 20
                            always (equal (multiple-value-list (mstrick-arg-subdiff mstrick diff))
                                          (multiple-value-list (pl-arg-subdiff pl diff))))))
            ((11 12)
             ;; left-cum or right-cum with various c values
             ;; With 1/3 probability, rollback (pl stays unchanged, history preserved)
             (let ((c (- (random 20) 10))
                   (do-rollback (zerop (random 2))))
               (if (zerop (random 2))
                   (let ((rest-part (mstrick-left-cum mstrick c)))
                     (if do-rollback
                         (mstrick-left-cum-rollback mstrick rest-part)
                         (progn
                           ;; (format t "~%left-cum: ~D" c)
                           (pl-left-cum pl c)
                           (setq add-history nil))))
                   (let ((rest-part (mstrick-right-cum mstrick c)))
                     (if do-rollback
                         (mstrick-right-cum-rollback mstrick rest-part)
                         (progn
                           ;; (format t "~%right-cum: ~D" c)
                           (pl-right-cum pl c)
                           (setq add-history nil)))))))
            ((13 14)
             ;; shift operation (invalidates history)
             (let* ((ldelta (- (random 10) 5))
                    (rdelta (+ ldelta (random 5))))
               ;; (format t "~%shift: ~D ~D" ldelta rdelta)
               (mstrick-shift mstrick ldelta rdelta)
               (pl-shift pl ldelta rdelta))
             (setq add-history nil))
            ((15 16)
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
