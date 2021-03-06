(defpackage :cp/incremental-lp
  (:use :cl :cp/two-phase-simplex)
  (:import-from :cp/two-phase-simplex #:simplex-float #:+zero+ #:%dual-simplex!)
  (:export #:make-lp #:lp-add-row! #:lp-pop-row! #:lp-render #:lp-solve!)
  (:documentation "Provides warm-start LP solver for dynamically added
constraints, using dual simplex method."))
(in-package :cp/incremental-lp)

(defstruct (lp (:constructor %make-lp))
  (mat nil :type (simple-array simplex-float (* *)))
  (b nil :type (simple-array simplex-float (*)))
  (c nil :type (simple-array simplex-float (*)))
  (c-saved nil :type (simple-array simplex-float (*)))
  (dict nil :type (simple-array fixnum (*)))
  (row-stack nil :type list)
  (b-stack nil :type list)
  (prev-state nil :type (member nil :optimal :unbounded :infeasible)))

(defun make-lp (c &optional init-mat init-b)
  (declare (optimize (speed 3)))
  (declare ((simple-array simplex-float (*)) c)
           ((or null (simple-array simplex-float (*))) init-b)
           ((or null (simple-array simplex-float (* *))) init-mat))
  (when init-mat
    (assert (= (array-dimension init-mat 1) (length c)))
    (assert (and init-b (= (array-dimension init-mat 0) (length init-b)))))
  (let* ((n (length c))
         (m (if init-b (length init-b) 0))
         (mat (or init-mat (make-array (list 0 n) :element-type 'simplex-float)))
         (b (or init-b (make-array 0 :element-type 'simplex-float)))
         (c-saved (copy-seq c))
         (dict (make-array (+ n m) :element-type 'fixnum :initial-element 0)))
    (dotimes (i (length dict))
      (setf (aref dict i) i))
    (%make-lp :mat mat :b b :c c :c-saved c-saved :dict dict)))

(defun lp-add-row! (lp arow b)
  "Destructively adds a new `lazy' constraint ax <= b to LP. You can rely on the
side effect. Here `lazy' means you need to call LP-RENDER to create a new LP to
which all the added constraints are reflected."
  (declare (optimize (speed 3))
           ((simple-array simplex-float (*)) arow)
           (simplex-float b))
  (assert (= (length arow) (length (lp-c lp))))
  (push arow (lp-row-stack lp))
  (push b (lp-b-stack lp))
  lp)

(defun lp-pop-row! (lp)
  "Destructively deletes the last added lazy constraint."
  (pop (lp-row-stack lp))
  (pop (lp-b-stack lp))
  lp)

(defun lp-render (lp)
  "Creates a new LP to which all the added constraints are reflected."
  (declare (optimize (speed 3)))
  (destructuring-bind (m n) (array-dimensions (lp-mat lp))
    (declare ((mod #.array-dimension-limit) m n))
    (let* ((mat (lp-mat lp))
           (b (lp-b lp))
           (c (lp-c lp))
           (dict (lp-dict lp))
           (row-stack (lp-row-stack lp))
           (b-stack (lp-b-stack lp))
           (md (length row-stack))
           (new-m (+ m md))
           (new-mat (make-array (list new-m n) :element-type 'simplex-float :initial-element +zero+))
           (new-b (make-array new-m :element-type 'simplex-float :initial-element +zero+))
           (new-c (copy-seq c))
           (new-dict (make-array (+ n new-m) :element-type 'fixnum))
           (poses (make-array (+ n new-m) :element-type 'fixnum)))
      (declare ((mod #.array-dimension-limit) new-m))
      ;; new-dict
      (loop for i from (+ n m) below (length new-dict)
            do (setf (aref new-dict i) i))
      (replace new-dict dict)
      ;; new-b
      (replace new-b b)
      ;; new-mat
      (replace (sb-ext:array-storage-vector new-mat) (sb-ext:array-storage-vector mat))
      (dotimes (i (length poses))
        (setf (aref poses (aref new-dict i)) i))
      (loop for i from m below new-m
            for arow of-type (simple-array simplex-float (*)) in row-stack
            for bval of-type simplex-float in b-stack
            do (setf (aref new-b i) bval)
               (dotimes (j n)
                 (let ((coef (aref arow j))
                       (pos (aref poses j)))
                   (if (< pos n)
                       ;; xj is non-basic
                       (let ((col pos))
                         (incf (aref new-mat i col) coef))
                       ;; xj is basic
                       (let ((row (- pos n)))
                         (dotimes (j n)
                           (decf (aref new-mat i j) (* coef (aref mat row j))))
                         (decf (aref new-b i) (* coef (aref b row))))))))
      (%make-lp :mat new-mat :b new-b :c new-c :dict new-dict
                :c-saved (lp-c-saved lp) :prev-state (lp-prev-state lp)))))

(defun calc-obj (coefs primal)
  (declare (optimize (speed 3))
           ((simple-array simplex-float (*)) coefs primal))
  (let ((res +zero+))
    (declare (simplex-float res))
    (loop for c across coefs
          for x across primal
          do (incf res (* c x)))
    res))

(declaim (ftype (function * (values (or simplex-float (member :unbounded :infeasible))
                                    (or null (simple-array simplex-float (*)))
                                    (or null (simple-array simplex-float (*)))
                                    &optional))
                lp-solve!))
(defun lp-solve! (lp)
  "Maximizes cx subject to Ax <= b and x >= 0, and returns three values. See
docs of `cp/two-phase-simplex:dual-primal!' for the information of returned
value."
  (declare (optimize (speed 3)))
  (assert (null (lp-row-stack lp)))
  (let* ((mat (lp-mat lp))
         (b (lp-b lp))
         (c (lp-c lp))
         (dict (lp-dict lp))
         (c-saved (lp-c-saved lp)))
    (ecase (lp-prev-state lp)
      ((:infeasible) (values :infeasible nil nil))
      ((nil :unbounded)
       (multiple-value-bind (res primal dual) (dual-primal! mat b c dict)
         (if (numberp res)
             (let ((obj (calc-obj c-saved primal)))
               (setf (lp-prev-state lp) :optimal)
               (values obj primal dual))
             (progn
               (setf (lp-prev-state lp) res)
               (values res nil nil)))))
      ((:optimal)
       (multiple-value-bind (res primal dual) (%dual-simplex! mat b c dict)
         (cond ((numberp res)
                (setf (lp-prev-state lp) :optimal)
                (values (calc-obj c-saved primal) primal dual))
               ((eql res :infeasible)
                (values res nil nil))
               (t (error "Huh?"))))))))
