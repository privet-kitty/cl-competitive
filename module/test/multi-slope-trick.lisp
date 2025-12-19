(defpackage :cp/test/multi-slope-trick
  (:use :cl :fiveam :cp/multi-slope-trick :cp/bisect)
  (:import-from :cp/multi-slope-trick
                #:mset #:%mset-key #:%mset-left #:%mset-right #:%mset-count
                #:%mset-concat #:mset-concat #:mset-split #:mset-indexed-split
                #:mset-insert #:mset-map-run-length #:mset-shift
                #:mset-first #:mset-last #:mset-size #:force-down #:update-size
                #:%mstrick-ltree #:%mstrick-rtree)
  (:import-from :cp/test/base #:base-suite))
(in-package :cp/test/multi-slope-trick)
(in-suite base-suite)

(declaim (inline mset-find))
(defun mset-find (mset key)
  "Finds and returns KEY if it exists, otherwise returns NIL. Equality is here
equivalent to 'neither larger nor smaller'."
  (declare ((or null mset) mset))
  (labels ((recur (mset)
             (unless mset
               (return-from recur nil))
             (force-down mset)
             (cond ((< key (%mset-key mset))
                    (recur (%mset-left mset)))
                   ((< (%mset-key mset) key)
                    (recur (%mset-right mset)))
                   (t key))))
    (recur mset)))

(declaim (inline mset-ref))
(defun mset-count (mset key)
  "Returns the number of KEYs in MSET."
  (declare ((or null mset) mset))
  (labels ((recur (mset)
             (unless mset
               (return-from recur 0))
             (force-down mset)
             (cond ((< key (%mset-key mset))
                    (recur (%mset-left mset)))
                   ((< (%mset-key mset) key)
                    (recur (%mset-right mset)))
                   (t (%mset-count mset)))))
    (recur mset)))

(test translatable-multiset/random
  (let ((*random-state* (sb-ext:seed-random-state 0))
        (*test-dribble* nil)
        (counter (make-hash-table :test #'eq)))
    (dotimes (_ 20)
      (let (mset
            (vector (make-array 0 :element-type 'fixnum)))
        (dotimes (_ 300)
          (ecase (random 4)
            ;; push
            (0 (let ((key (random 15))
                     (count (+ 1 (random 3))))
                 (setq mset (mset-insert mset key count))
                 (let ((pos (bisect-left vector key)))
                   (setq vector (concatenate
                                 '(simple-array fixnum (*))
                                 (subseq vector 0 pos)
                                 (make-array count :initial-element key)
                                 (subseq vector pos))))))
            ;; split and concat
            (1
             (let ((key (random 15)))
               (multiple-value-bind (l r) (mset-split mset key)
                 (let ((shiftl (- (random 6) 3))
                       (shiftr (- (random 6) 3)))
                   (when (> shiftl shiftr)
                     (rotatef shiftl shiftr))
                   (when l
                     (mset-shift l shiftl)
                     (loop for i below (mset-size l)
                           do (incf (aref vector i) shiftl)))
                   (when r
                     (mset-shift r shiftr)
                     (loop for i below (mset-size r)
                           do (incf (aref vector (+ (mset-size l) i)) shiftr))))
                 (setq mset (%mset-concat l r))))
             (let ((index (random (+ 1 (length vector)))))
               (multiple-value-bind (l r) (mset-indexed-split mset index)
                 (let ((shiftl (- (random 6) 3))
                       (shiftr (- (random 6) 3)))
                   (when (> shiftl shiftr)
                     (rotatef shiftl shiftr))
                   (when l
                     (mset-shift l shiftl)
                     (loop for i below (mset-size l)
                           do (incf (aref vector i) shiftl)))
                   (when r
                     (mset-shift r shiftr)
                     (loop for i below (mset-size r)
                           do (incf (aref vector (+ (mset-size l) i)) shiftr))))
                 (setq mset (mset-concat l r)))))
            ;; search
            (2
             (let ((key (if (or (zerop (length vector)) (zerop (random 3)))
                            (random 15)
                            (aref vector (random (length vector))))))
               (is (eql (mset-find mset key) (find key vector)))
               (is (eql (mset-count mset key) (count key vector)))
               (is (= (mset-size mset) (length vector)))
               (when (> (length vector) 0)
                 (is (= (%mset-key (mset-first mset)) (aref vector 0)))
                 (is (= (%mset-key (mset-last mset)) (aref vector (- (length vector) 1)))))))
            ;; map
            (3
             (clrhash counter)
             (let ((prev most-negative-fixnum))
               (mset-map-run-length
                (lambda (key count)
                  (declare (ignore count))
                  (is (< prev key))
                  (setq prev key)
                  (incf (the fixnum (gethash key counter 0))))
                mset))
             (is (loop for x being each hash-value of counter
                       always (<= x 1))))))))))

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

(defun pl-ensure-breakpoint (pl a)
  "Ensures there's a breakpoint at A. Returns the position."
  (let ((breakpoints (%pl-breakpoints pl))
        (slopes (%pl-slopes pl)))
    (let ((index (bisect-left breakpoints a)))
      (unless (and (< index (length breakpoints))
                   (= a (aref breakpoints index)))
        ;; Insert breakpoint
        (vector-push-extend a breakpoints)
        (loop for i from (1- (length breakpoints)) downto (1+ index)
              do (setf (aref breakpoints i) (aref breakpoints (1- i))))
        ;; Insert slope (copy from current interval)
        (vector-push-extend (aref slopes (1+ index)) slopes)
        (loop for i from (1- (length slopes)) downto (+ index 2)
              do (setf (aref slopes i) (aref slopes (1- i)))))
      index)))

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

(defun pl-add-both (pl a weight)
  "Adds weight*|x-a| to f."
  (pl-add pl a weight)
  (pl-add pl a (- weight)))

(defun pl-left-cum (pl)
  "g(x) = min_{t <= x} f(t).
Sets all positive slopes to 0."
  (let ((slopes (%pl-slopes pl)))
    (dotimes (i (length slopes))
      (when (> (aref slopes i) 0)
        (setf (aref slopes i) 0))))
  (pl-merge pl))

(defun pl-right-cum (pl)
  "g(x) = min_{x <= t} f(t).
Sets all negative slopes to 0."
  (let ((slopes (%pl-slopes pl)))
    (dotimes (i (length slopes))
      (when (< (aref slopes i) 0)
        (setf (aref slopes i) 0))))
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
                 ;; This shouldn't happen after merge, but handle anyway
                 (t
                  (vector-push-extend (+ bp ldelta) new-bp)
                  (vector-push-extend right-slope new-slopes)))))
           (setf (%pl-breakpoints pl) new-bp
                 (%pl-slopes pl) new-slopes)
           (pl-merge pl))))))
  pl)

(defun pl-argmin (pl)
  "Returns the interval [left, right] where the function is minimized.
Returns NIL for unbounded directions."
  (let ((breakpoints (%pl-breakpoints pl))
        (slopes (%pl-slopes pl)))
    (if (zerop (length breakpoints))
        (values nil nil)
        (let ((left-end nil)
              (right-end nil))
          ;; left-end: breakpoint where slope becomes non-negative
          (loop for i from 0 below (length breakpoints)
                when (and (< (aref slopes i) 0)
                          (>= (aref slopes (1+ i)) 0))
                do (setq left-end (aref breakpoints i))
                   (return))
          ;; right-end: breakpoint where slope becomes positive
          (loop for i from (1- (length breakpoints)) downto 0
                when (and (<= (aref slopes i) 0)
                          (> (aref slopes (1+ i)) 0))
                do (setq right-end (aref breakpoints i))
                   (return))
          (values left-end right-end)))))

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

(defun pl-equal (pl1 pl2)
  "Checks if two pl-slope-tricks represent the same function."
  (and (equalp (%pl-breakpoints pl1) (%pl-breakpoints pl2))
       (equalp (%pl-slopes pl1) (%pl-slopes pl2))))


(test slope-trick-operation/random
  (let ((*random-state* (sb-ext:seed-random-state 0))
        (*test-dribble* nil))
    (dotimes (i 500)
      (let ((mstrick (make-multi-slope-trick))
            (pl (make-pl))
            (add-history nil))
        ;; (format t "~%case: ~D" i)
        (dotimes (_ 50)
          ;; (format t "~%state: ~A" mstrick)
          (ecase (random 14)
            ((0 1 2 3)
             ;; add
             (let ((a (- (random 20) 10))
                   (weight (- (random 20) 10)))
               ;; (format t "~%add: ~D ~D" a weight)
               (mstrick-add mstrick a weight)
               (pl-add pl a weight)
               (push (cons a weight) add-history)))
            ((4 5 6 7)
             ;; add-both (record as two separate adds)
             (let ((a (- (random 20) 10))
                   (weight (random 10)))
               ;; (format t "~%add: ~D ~D" a weight)
               ;; (format t "~%add: ~D ~D" a (- weight))
               (mstrick-add-both mstrick a weight)
               (pl-add-both pl a weight)
               (push (cons a weight) add-history)
               (push (cons a (- weight)) add-history)))
            ((8 9)
             ;; check subdiff and argmin
             (loop for x from -21 to 21
                   do (is (equal (multiple-value-list (mstrick-subdiff mstrick x))
                                 (multiple-value-list (pl-subdiff pl x)))))
             (is (equal (multiple-value-list (mstrick-argmin mstrick))
                        (multiple-value-list (pl-argmin pl)))))
            (10
             ;; left-cum or right-cum (invalidates history)
             (if (zerop (random 2))
                 (progn
                   (mstrick-left-cum mstrick)
                   (pl-left-cum pl))
                 (progn
                   (mstrick-right-cum mstrick)
                   (pl-right-cum pl)))
             (setq add-history nil))
            (11
             ;; shift operation (invalidates history)
             (let* ((ldelta (- (random 10) 5))
                    (rdelta (+ ldelta (random 5))))
               (mstrick-shift mstrick ldelta rdelta)
               (pl-shift pl ldelta rdelta))
             (setq add-history nil))
            ((12 13)
             ;; delete (rollback a random add from history)
             (when add-history
               (let* ((idx (random (length add-history)))
                      (entry (nth idx add-history))
                      (a (car entry))
                      (weight (cdr entry)))
                 ;; (format t "~%delete: ~D ~D" a weight)
                 (mstrick-delete mstrick a weight)
                 (pl-delete pl a weight)
                 (setq add-history
                       (nconc (subseq add-history 0 idx)
                              (nthcdr (+ idx 1) add-history))))))))))))
