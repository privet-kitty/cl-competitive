(defpackage :cp/bsgs
  (:use :cl :cp/gemm)
  (:export #:bsgs-group #:bsgs-semigroup #:bsgs-semigroup-cycle
           #:%cycle-length #:%cycle-start #:make-fbase)
  (:documentation
   "Provides solvers for discrete logarithm problem on (semi)group.

Reference:
S. Tinani, J. Rosenthal. A deterministic algorithm for the discrete logarithm
problem in a semigroup. 
C. Monico. Semirings and semigroup actions in public-key cryptography."))
(in-package :cp/bsgs)

;; NOTE: incomplete

(deftype uint () '(integer 0 #.most-positive-fixnum))

(defconstant +identity+
  (if (boundp '+identity+)
      (symbol-value '+identity+)
      (make-symbol "IDENTITY")))

(declaim (inline %power))
(defun %power (base exponent op)
  "OP := binary operation (comprising a semigroup)"
  (declare (uint exponent))
  (loop with res = +identity+
        while (> exponent 0)
        when (oddp exponent)
        do (setq res (if (eql res +identity+)
                         base
                         (funcall op res base)))
        do (setq base (funcall op base base)
                 exponent (ash exponent -1))
        finally (return res)))

(declaim (ftype (function * (values (or null uint) &optional)) bsgs-group))
(defun bsgs-group (f action op x y max &key (test #'eql) from-zero)
  "Finds the least positive (or non-negative, if FROM-ZERO is true) integer k
such that F^k * X = Y, where * is a left action. Time complexity is
O(sqrt(MAX)).

Args:
Let G be a group. F is an element of G. ACTION is a two-variable function that
expresses *. OP is an operation of G.

Time complexity:
Let N := MAX. Let T be the time complexity of the group action and let M be that
of multiplication on G. Then this function takes expected O(sqrt(N)T +
log(N)M) time.

TODO:
We can do this in O(sqrt(N)T) time per query if f-gs is given."
  (declare ((integer 1 #.most-positive-fixnum) max))
  (let* ((bsize (+ 1 (isqrt (- max 1))))
         (table (make-hash-table :size bsize :test test))
         (rvalue y)
         (f-gs (%power f bsize op)))
    (when (and from-zero (funcall test x y))
      (return-from bsgs-group 0))
    ;; baby step
    (dotimes (i bsize)
      (setf (gethash rvalue table) i
            rvalue (funcall action f rvalue)))
    ;; giant step
    (loop with value = x
          for i from 1 to bsize
          do (setq value (funcall action f-gs value))
             (let ((j (gethash value table)))
               (declare ((or null uint) j))
               (when j
                 (return (- (the uint (* i bsize)) j)))))))

(declaim (ftype (function * (values simple-vector &optional)) make-fbase))
(defun make-fbase (f op length)
  (declare (uint length))
  (let ((res (make-array length :element-type t :initial-element nil)))
    (dotimes (i length)
      (setf (aref res i)
            (if (zerop i)
                f
                (funcall op (aref res (- i 1)) (aref res (- i 1))))))
    res))

(defun %power-with-base (fbase op exp)
  (declare (simple-vector fbase)
           ((integer 1 #.most-positive-fixnum) exp))
  (let ((res +identity+))
    (dotimes (i (length fbase))
      (when (logbitp i exp)
        (setq res (if (eql res +identity+)
                      (aref fbase i)
                      (funcall op res (aref fbase i))))))
    res))

(defun %action-with-base (fbase action exp value)
  (declare (simple-vector fbase)
           (uint exp))
  (dotimes (i (length fbase))
    (when (logbitp i exp)
      (setq value (funcall action (aref fbase i) value))))
  value)

(declaim (ftype (function * (values uint &optional)) %cycle-length))
(defun %cycle-length (fbase f action op initial-value max &key (test #'eql))
  "Finds the cycle length of the sequence x, F*x, (F^2)*x, ...

Args:
Let S be a semigroup. F is an element of S. ACTION is a two-variable function
that expresses *. OP is an operation of S. INITIAL-VALUE is x. MAX is the upper
bound of the element order.

Time complexity:
Let N := MAX. Let T be the time complexity of the semigroup action and let M be
that of multiplication on S. Then this function takes expected O(sqrt(N)T +
log(N)M)."
  (declare ((integer 1 #.most-positive-fixnum) max))
  (let* ((bsize (+ 1 (isqrt (- max 1))))
         (table (make-hash-table :size bsize :test test))
         (f^n (%power-with-base fbase op max))
         (value (funcall action f^n initial-value)))
    ;; baby step
    (dotimes (i bsize)
      (let ((prev (gethash value table)))
        (declare ((or null uint) prev))
        (when prev
          (return-from %cycle-length (- i prev))))
      (setf (gethash value table) i
            value (funcall action f value)))
    ;; giant step
    (loop with f-gs = (%power-with-base fbase op bsize)
          for i from 1 to bsize
          for j of-type (or null uint) = (gethash value table)
          when j
          do (return-from %cycle-length (- (* i bsize) j))
          do (setq value (funcall action f-gs value)))
    (error "Cycle not found.")))

(declaim (ftype (function * (values uint &optional)) %cycle-start))
(defun %cycle-start (fbase cycle-length f action op initial-value max &key (test #'eql))
  "Finds the start point of cycle.

Time complexity:
Let N := MAX. Let T be the time complexity of the semigroup action and let M be
that of multiplication on S. Then this function takes O((T+M)log(n)) time."
  (declare ((integer 1 #.most-positive-fixnum) cycle-length max)
           (simple-vector fbase))
  (assert (<= cycle-length max))
  (let* ((fcycle (%power-with-base fbase op cycle-length))
         (value initial-value))
    ;; check initial condition of binary search
    (when (funcall test value (funcall action fcycle value))
      (return-from %cycle-start 0))
    (loop with res of-type uint = 0
          for i from (- (length fbase) 1) downto 0
          for new-value = (funcall action (aref fbase i) value)
          unless (funcall test new-value (funcall action fcycle new-value))
          do (setq value new-value)
             (incf res (ash 1 i))
          finally ;; just for assertion
             (setq value (funcall action f value))
             (assert (funcall test value (funcall action fcycle value)))
             (return (+ res 1)))))

(defun bsgs-semigroup (f action op x y max &key (test #'eql))
  "Finds the least **non-negative** integer k such that F^k * X = Y, where * is
a left action.

Args:
Let S be a semigroup. F is an element of S. ACTION is a two-variable function
that expresses *. OP is an operation of S.

Time complexity:
Let N := MAX. Let T be the time complexity of the semigroup action and let M be
that of multiplication on S. Then this function takes expected O(sqrt(N)T +
log(N)M) time.

TODO:
We can do this in O(sqrt(N)log(N)T) time per query with some preprocessing."
  (declare ((integer 1 #.most-positive-fixnum) max))
  (let* ((fbase-len (integer-length (sb-int:power-of-two-ceiling max))) ; FIXME: too large?
         (fbase (make-fbase f op fbase-len))
         (cycle-length (%cycle-length fbase f action op x max :test test))
         (fcycle (%power-with-base fbase op cycle-length))
         (fcycle-base (make-fbase fcycle op fbase-len))
         (cycle-start (%cycle-start fbase cycle-length f action op x max :test test))
         (tt (ceiling cycle-start cycle-length))
         (gen-exp (+ 1 (* tt cycle-length)))
         (f-gen (%power-with-base fbase op gen-exp))
         (yp y)
         (b 0))
    (declare (uint gen-exp b))
    (labels ((fail () (return-from bsgs-semigroup (values nil cycle-start cycle-length)))
             (in-cycle-p (z) (funcall test z (funcall action fcycle z))))
      (unless (in-cycle-p (%action-with-base fcycle-base action tt y))
        (fail))
      (unless (in-cycle-p y)
        (loop for i from (- fbase-len 1) downto 0
              for new-yp = (funcall action (aref fcycle-base i) yp)
              unless (in-cycle-p new-yp)
              do (setq yp new-yp)
                 (incf b (ash 1 i)))
        (setq yp (funcall action fcycle yp))
        (incf b))
      (let* ((mp (bsgs-group f-gen action op x yp cycle-length :test test :from-zero t))
             (diff 0))
        (declare (uint diff))
        (unless mp (fail))
        (multiple-value-bind (max-c rem) (floor (* mp gen-exp) cycle-length)
          (declare (uint max-c rem))
          (let ((value (%action-with-base fbase action rem x)))
            (unless (in-cycle-p value)
              (loop for i from (- fbase-len 1) downto 0
                    for new-value = (funcall action (aref fcycle-base i) value)
                    unless (in-cycle-p new-value)
                    do (setq value new-value)
                       (incf diff (ash 1 i))
                    finally (incf diff))))
          (let* ((c (- max-c diff))
                 (res (- (* mp gen-exp) (* (+ b c) cycle-length))))
            (when (and (>= res 0) (funcall test y (%action-with-base fbase action res x)))
              (return-from bsgs-semigroup (values res cycle-start cycle-length))))))
      (fail))))

(defun bsgs-semigroup-cycle (f action op x max &key (test #'eql))
  "Finds the least positive integer k such that F^k * X = X, where * is
a left action."
  (declare ((integer 1 #.most-positive-fixnum) max))
  (let* ((fbase-len (integer-length (sb-int:power-of-two-ceiling max))) ; FIXME: too large?
         (fbase (make-fbase f op fbase-len))
         (cycle-length (%cycle-length fbase f action op x max :test test)))
    (when (funcall test x (%action-with-base fbase action cycle-length x))
      (return-from bsgs-semigroup-cycle cycle-length))))
