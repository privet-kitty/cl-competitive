(defpackage :cp/lp
  (:use :cl :cp/csc :cp/extend-vector :cp/lud :cp/sparse-simplex)
  (:import-from :cp/csc #:csc-float #:+zero+ #:+one+)
  (:import-from :cp/sparse-simplex #:%make-sparse-lp)
  (:export #:lp-var #:new-lp-var #:lp-var-lo #:lp-var-up #:lp-var-name
           #:linear-expr #:linear-expr-add-var
           #:lp-constraint #:new-lp-constr
           #:lp-problem #:make-lp-problem #:lp-problem-objective #:lp-problem-sense
           #:lp-problem-obj-value #:lp-problem-status
           #:lp-problem-value #:lp-problem-solve)
  (:documentation "Provides modelling tools for general form LP."))
(in-package :cp/lp)

(deftype index () '(mod #.array-dimension-limit))

(defvar *uid* 0)
(defun get-uid ()
  (incf *uid*))

(defstruct (lp-var (:constructor %make-lp-var))
  (lo nil :type (or null csc-float))
  (up nil :type (or null csc-float))
  (column nil :type (or index (cons index index)))
  (row nil :type (or null index))
  (name (get-uid) :type (or index string)))

(defmethod print-object ((lp-var lp-var) stream)
  (if *print-escape*
      (call-next-method)
      (let ((name (lp-var-name lp-var)))
        (if (numberp name)
            (format stream "x(~A)" name)
            (print-object name stream)))))

(defstruct (linear-expr (:constructor %make-linear-expr))
  (nz 0 :type index)
  (coefs nil :type (simple-array csc-float (*)))
  (vars nil :type simple-vector))

(defmethod print-object ((linear-expr linear-expr) stream)
  (if *print-escape*
      (call-next-method)
      (let ((nz (linear-expr-nz linear-expr))
            (coefs (linear-expr-coefs linear-expr))
            (vars (linear-expr-vars linear-expr)))
        (if (zerop nz)
            (write-char #\0 stream)
            (dotimes (i nz)
              (unless (zerop i)
                (write-string " + " stream))
              (format stream "~A * ~A" (aref coefs i) (aref vars i)))))))

(defstruct (lp-constraint (:constructor %make-lp-constraint))
  (lo nil :type (or null csc-float))
  (up nil :type (or null csc-float))
  (row nil :type (or null index (cons index index)))
  (column nil :type (or null index (cons index index)))
  (expr nil :type linear-expr))

(defmethod print-object ((lp-constraint lp-constraint) stream)
  (if *print-escape*
      (call-next-method)
      (let ((lo (lp-constraint-lo lp-constraint))
            (up (lp-constraint-up lp-constraint))
            (expr (lp-constraint-expr lp-constraint)))
        (when lo
          (format stream " ~A <= " lo))
        (write expr :stream stream)
        (when up
          (format stream " <= ~A" up)))))

(defstruct lp-problem
  (sense 1 :type (integer -1 1)) ; maximize: 1, minimize: -1
  (m 0 :type index) ; number of rows
  (n 0 :type index) ; number of columns
  (objective (linear-expr) :type linear-expr)
  (nconstr 0 :type index)
  (constraints (make-array 1 :element-type t :initial-element nil) :type simple-vector)
  (nvar 0 :type index)
  (variables (make-array 1 :element-type t :initial-element nil) :type simple-vector)
  (slack-cols (make-array 0 :element-type 'index :fill-pointer 0) :type (vector index))
  ;; Results
  (last-basis nil :type (or null vector))
  (status :not-solved :type lp-status)
  (obj-value +zero+ :type csc-float)
  (primal-sol nil :type (or null (simple-array csc-float (*))))
  (dual-sol nil :type (or null (simple-array csc-float (*)))))

(defmethod print-object ((lp-problem lp-problem) stream)
  (if *print-escape*
      (call-next-method)
      (progn
        (write-string (ecase (lp-problem-sense lp-problem)
                        (-1 "min.")
                        (1 "max."))
                      stream)
        (write-char #\Space stream)
        (print-object (lp-problem-objective lp-problem) stream)
        (format stream "~&s. t.")
        (let ((nconstr (lp-problem-nconstr lp-problem))
              (constraints (lp-problem-constraints lp-problem)))
          (dotimes (i nconstr)
            (fresh-line stream)
            (print-object (aref constraints i) stream)))
        (let ((nvar (lp-problem-nvar lp-problem))
              (vars (lp-problem-variables lp-problem)))
          (dotimes (i nvar)
            (fresh-line stream)
            (let* ((var (aref vars i))
                   (lo (lp-var-lo var))
                   (up (lp-var-up var)))
              (when lo
                (format stream "~A <= " lo))
              (write var :stream stream)
              (when up
                (format stream " <= ~A" up))))))))

(declaim (inline get-column))
(defun get-column (problem)
  (declare (optimize (speed 3)))
  (- (incf (lp-problem-n problem)) 1))

(declaim (inline get-row))
(defun get-row (problem)
  (declare (optimize (speed 3)))
  (- (incf (lp-problem-m problem)) 1))

(defun new-lp-var (problem lo up &optional (name (get-uid)))
  (declare (optimize (speed 3))
           ((or null index string)))
  (labels ((%get-column ()
             (let ((slack-col (get-column problem)))
               (vector-push-extend slack-col (lp-problem-slack-cols problem))
               slack-col)))
    (let ((column
            (cond ((and lo up) (cons (get-column problem) (%get-column)))
                  ((or lo up) (get-column problem))
                  (t (cons (get-column problem) (get-column problem)))))
          (row (when (and lo up)
                 (get-row problem))))
      (symbol-macrolet ((nvar (lp-problem-nvar problem))
                        (variables (lp-problem-variables problem)))
        (prog1
            (vector-set* variables nvar
                         (%make-lp-var :lo lo :up up :column column :row row :name name))
          (incf nvar))))))

(declaim (inline linear-expr))
(defun linear-expr (&optional coefs vars)
  (declare (sequence coefs vars))
  (assert (= (length coefs) (length vars)))
  (let ((nz (or (position nil coefs) (length coefs))))
    (%make-linear-expr :nz nz
                       :coefs (coerce coefs '(simple-array csc-float (*)))
                       :vars (coerce vars 'simple-vector))))

(defun linear-expr-add-var (linear-expr coef var)
  (declare (optimize (speed 3)))
  (symbol-macrolet ((nz (linear-expr-nz linear-expr))
                    (coefs (linear-expr-coefs linear-expr))
                    (vars (linear-expr-vars linear-expr)))
    (vector-set* vars nz var)
    (vector-set* coefs nz coef)
    (incf nz)
    linear-expr))

(defun new-lp-constr (problem linear-expr lo up)
  (declare ((or null linear-expr) linear-expr))
  (labels ((%get-column ()
             (let ((slack-col (get-column problem)))
               (vector-push-extend slack-col (lp-problem-slack-cols problem))
               slack-col)))
    (multiple-value-bind (row column)
        (cond ((and lo up)
               (values (cons (get-row problem) (get-row problem))
                       (cons (%get-column) (%get-column))))
              ((or lo up)
               (values (get-row problem) (%get-column)))
              (t (values nil nil)))
      (symbol-macrolet ((nconstr (lp-problem-nconstr problem))
                        (constraints (lp-problem-constraints problem)))
        (prog1
            (vector-set* constraints nconstr
                         (%make-lp-constraint
                          :lo lo :up up
                          :row row :column column
                          :expr linear-expr))
          (incf nconstr))))))

(declaim (inline %power-of-two-ceiling))
(defun %power-of-two-ceiling (x)
  (ash 1 (integer-length (- x 1))))

(declaim (ftype (function * (values csc-float &optional)) var-offset))
(defun var-offset (var)
  (declare (optimize (speed 3)))
  (or (lp-var-lo var)
      (lp-var-up var)
      +zero+))

(declaim (ftype (function * (values csc (simple-array csc-float (*)) &optional))
                to-standard-ab))
(defun to-standard-ab (problem)
  "Transforms general form (max. cx s. t. L <= Ax <= U, l <= x <= u) to standard
form (max. cx Ax <= b, x >= 0)."
  (declare (optimize (speed 3)))
  (let* ((m (lp-problem-m problem))
         (n (lp-problem-n problem))
         ;; TODO: Creating transposed COO first will be faster as this COO is
         ;; already sorted w.r.t. row.
         (coo (make-coo m n (the index (+ 1 m n))))
         (b (make-array m :element-type 'csc-float :initial-element +zero+))
         (vars (lp-problem-variables problem))
         (constraints (lp-problem-constraints problem)))
    (dotimes (i (lp-problem-nconstr problem))
      (let* ((constr (aref constraints i))
             (lo (lp-constraint-lo constr))
             (up (lp-constraint-up constr))
             (crow (lp-constraint-row constr))
             (main-crow (if (listp crow) (car crow) crow))
             (ccolumn (lp-constraint-column constr))
             (expr (lp-constraint-expr constr))
             (nz (linear-expr-nz expr))
             (coefs (linear-expr-coefs expr))
             (vars (linear-expr-vars expr)))
        (declare ((or null csc-float) lo up))
        (when (or lo up)
          (dotimes (k nz)
            (let* ((coef (aref coefs k))
                   (var (aref vars k))
                   (vcolumn (lp-var-column var))
                   (vlo (lp-var-lo var))
                   (vup (lp-var-up var))
                   (voffset (var-offset var))
                   (vsign (if (and (not vlo) vup) -1 1)))
              (cond ((and vlo vup)
                     (coo-insert! coo main-crow (car vcolumn) (* vsign coef)))
                    ((or vlo vup)
                     (coo-insert! coo main-crow vcolumn (* vsign coef)))
                    (t
                     (coo-insert! coo main-crow (car vcolumn) (* vsign coef))
                     (coo-insert! coo main-crow (cdr vcolumn) (- (* vsign coef)))))
              (when lo
                (decf lo (* coef voffset)))
              (when up
                (decf up (* coef voffset)))))
          ;; additional rows and columns for the constraint
          (cond ((and lo up)
                 ;; sum(a_i*x_i) - z1 = L, z1 + z2 = U - L
                 (destructuring-bind (crow1 . crow2) crow
                   (destructuring-bind (ccolumn1 . ccolumn2) ccolumn
                     (coo-insert! coo crow1 ccolumn1 (- +one+))
                     (coo-insert! coo crow2 ccolumn1 +one+)
                     (coo-insert! coo crow2 ccolumn2 +one+)
                     (setf (aref b crow1) lo
                           (aref b crow2) (- up lo)))))
                (lo ;; sum(a_i*x_i) - z = L
                 (coo-insert! coo crow ccolumn (- +one+))
                 (setf (aref b crow) lo))
                (up ;; sum(a_i*x_i) + z = U
                 (coo-insert! coo crow ccolumn +one+)
                 (setf (aref b crow) up))))))
    ;; additional row & column for (l <= x <= u)-type variables
    (dotimes (j (lp-problem-nvar problem))
      (let* ((var (aref vars j))
             (lo (lp-var-lo var))
             (up (lp-var-up var)))
        (when (and lo up)
          (destructuring-bind (vcolumn1 . vcolumn2) (lp-var-column var)
            (let ((vrow (lp-var-row var)))
              (coo-insert! coo vrow vcolumn1 +one+)
              (coo-insert! coo vrow vcolumn2 +one+)
              (setf (aref b vrow) (- up lo)))))))
    (let ((a (make-csc-from-coo coo)))
      (values a b))))

(declaim (ftype (function * (values (simple-array csc-float (*)) csc-float &optional))
                to-standard-c))
(defun to-standard-c (problem)
  (declare (optimize (speed 3)))
  (let* ((n (lp-problem-n problem))
         (c (make-array n :element-type 'csc-float :initial-element +zero+))
         (obj-offset +zero+)
         (objective (lp-problem-objective problem))
         (nz (linear-expr-nz objective))
         (coefs (linear-expr-coefs objective))
         (vars (linear-expr-vars objective)))
    (declare (csc-float obj-offset))
    (dotimes (k nz)
      (let* ((coef (aref coefs k))
             (var (aref vars k))
             (vcolumn (lp-var-column var))
             (vlo (lp-var-lo var))
             (vup (lp-var-up var))
             (voffset (var-offset var))
             (vsign (if (and (not vlo) vup) -1 1)))
        (cond ((and vlo vup)
               (setf (aref c (car vcolumn)) (* vsign coef)))
              ((or vlo vup)
               (setf (aref c vcolumn) (* vsign coef)))
              (t
               (setf (aref c (car vcolumn)) (* vsign coef))
               (setf (aref c (cdr vcolumn)) (- (* vsign coef)))))
        (incf obj-offset (* coef voffset))))
    (when (eql -1 (lp-problem-sense problem))
      (dotimes (j n)
        (setf (aref c j) (- (aref c j))))
      (setq obj-offset (- obj-offset)))
    (values c obj-offset)))

(defun lp-problem-slp (problem &optional warm-start)
  (declare (optimize (speed 3))
           ((or null (eql t) vector) warm-start))
  (multiple-value-bind (a b) (to-standard-ab problem)
    (multiple-value-bind (c obj-offset) (to-standard-c problem)
      (let* ((m (lp-problem-m problem))
             (n (lp-problem-n problem))
             (slack-cols (lp-problem-slack-cols problem))
             ;; TODO: warm-start
             (basis (case warm-start
                      ((nil) slack-cols)
                      ((t) (let ((last-basis (lp-problem-last-basis problem))
                                 (new-basis (copy-seq slack-cols)))
                             (assert last-basis)
                             (replace new-basis last-basis)
                             new-basis))
                      (otherwise
                       (let ((new-basis (copy-seq slack-cols)))
                         (replace new-basis warm-start)
                         new-basis))))
             (dictionary (make-dictionary m n basis))
             (x-basic (make-array m :element-type 'csc-float))
             (y-nonbasic (make-array n :element-type 'csc-float))
             (a-transposed (csc-transpose a)))
        (assert (= (length slack-cols) m))
        (dotimes (i m)
          (setf (aref x-basic i) (aref b i)))
        (let* ((lude (refactor a basis))
               (slp (%make-sparse-lp :m m :n n
                                     :mat a :tmat a-transposed
                                     :b b :c c
                                     :x-basic x-basic
                                     :y-nonbasic y-nonbasic
                                     :dictionary dictionary
                                     :lude lude
                                     :obj-offset obj-offset)))
          (correct-x-basic! lude x-basic)
          (correct-y-nonbasic! slp)
          slp)))))

(defun lp-problem-solve (problem solver &optional warm-start)
  "Condition that you can warm-start the LP solver:
- No values appearing in the LP are not changed from (to) a finite real number
to (from) an infinity. (This is not a theoretical limitation, but due to
implementation reasons.)"
  (declare ((function (sparse-lp) (values lp-status)) solver)
           ((or null (eql t) vector) warm-start))
  (let* ((slp (lp-problem-slp problem warm-start))
         (status (funcall solver slp)))
    (setf (lp-problem-status problem) status
          (lp-problem-last-basis problem) (dictionary-basis (slp-dictionary slp)))
    (setf (values (lp-problem-obj-value problem)
                  (lp-problem-primal-sol problem)
                  (lp-problem-dual-sol problem))
          (slp-restore slp))
    (setf (lp-problem-obj-value problem)
          (* (lp-problem-sense problem)
             (lp-problem-obj-value problem)))
    status))

(defun lp-problem-value (problem var)
  (declare (optimize (speed 3)))
  (let ((lo (lp-var-lo var))
        (up (lp-var-up var))
        (sol (lp-problem-primal-sol problem))
        (column (lp-var-column var)))
    (cond ((and lo up)
           (+ (aref sol (car column)) lo))
          (lo (+ (aref sol column) lo))
          (up (- up (aref sol column)))
          (t (- (aref sol (car column)) (aref sol (cdr column)))))))


(defun test ()
  (let* ((problem (make-lp-problem :sense -1))
         (xs (vector (new-lp-var problem -2d0 nil "x1")
                     (new-lp-var problem 0d0 6d0 "x2")))
         (constr (vector (new-lp-constr problem
                                        (linear-expr '(-1d0 1d0) xs)
                                        1d0 5d0)
                         (new-lp-constr problem
                                        (linear-expr '(-3d0 2d0) xs)
                                        2d0 10d0)
                         (new-lp-constr problem
                                        (linear-expr '(2d0 -1d0) xs)
                                        nil 0d0))))
    (setf (lp-problem-objective problem)
          (linear-expr '(3d0 -1d0) xs))
    (princ problem)
    (multiple-value-bind (a b) (to-standard-ab problem)
      (multiple-value-bind (c obj-offset) (to-standard-c problem)
        (let ((status (lp-problem-solve problem #'slp-self-dual!)))
          (values status
                  (lp-problem-obj-value problem)
                  (lp-problem-primal-sol problem)
                  (map 'list (lambda (v) (lp-problem-value problem v)) xs)))))))
