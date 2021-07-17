(defpackage :cp/lu-decomposition
  (:use :cl :cp/csc #:cp/movable-binary-heap #:cp/iterset #:cp/rdtscp #:cp/extend-vector)
  (:import-from :cp/csc #:csc-float #:+zero+ #:+one+)
  (:export #:lud #:lud-eta #:lu-factor
           #:lud-lower #:lud-upper #:lud-tlower #:lud-tupper #:lud-diagu
           #:lud-rank #:lud-colperm #:lud-icolperm #:lud-rowperm #:lud-irowperm #:lud-m
           #:make-lud-eta #:dense-solve! #:add-eta! #:lud-eta-lud
           #:refactor #:refactor-p #:*refactor-threshold* #:*refactor-by-time*
           #:gauss-eta! #:gauss-eta-transposed!
           #:sparse-solve! #:sparse-solve-transposed!)
  (:documentation "Reference:
Robert J. Vanderbei. Linear Programming: Foundations and Extensions. 5th edition."))
(in-package :cp/lu-decomposition)

;; NOTE: Incomplete

(defconstant +eps+ (coerce 1d-14 'csc-float))
(defconstant +epsnum+ (coerce 1d-9 'csc-float))

(defstruct (lud (:conc-name lud-))
  (m nil :type (mod #.array-dimension-limit))
  ;; lower triangular matrix
  (lower nil :type csc)
  ;; transposition of LOWER
  (tlower nil :type csc)
  ;; upper triangular matrix
  (upper nil :type csc)
  ;; transposition of UPPER
  (tupper nil :type csc)
  ;; diagonal elements of upper triagular matrix
  (diagu nil :type (simple-array csc-float (*)))
  (rank nil :type (mod #.array-dimension-limit))
  (colperm nil :type (simple-array fixnum (*)))
  (icolperm nil :type (simple-array fixnum (*)))
  (rowperm nil :type (simple-array fixnum (*)))
  (irowperm nil :type (simple-array fixnum (*))))

(defmacro vector-set* (vector index new-element)
  (let ((i (gensym))
        (elm (gensym)))
    `(let ((,i ,index)
           (,elm ,new-element))
       (extend-vectorf ,vector (+ 1 ,i))
       (setf (aref ,vector ,i) ,elm))))

#+swank (set-dispatch-macro-character #\# #\> #'cl-debug-print:debug-print-reader)

(deftype ivec () '(simple-array fixnum (*)))
(deftype fvec () '(simple-array csc-float (*)))

(defconstant +nan+ -1)

(declaim (inline %power-of-two-ceiling))
(defun %power-of-two-ceiling (x)
  (ash 1 (integer-length (- x 1))))

(defun lu-factor (matrix basis)
  (declare (optimize (speed 3))
           (vector basis))
  ;; B: submatrix of MATRIX w.r.t. BASIS
  ;; BT: transposition of B
  ;; #>matrix
  (let* ((m (csc-m matrix))
         (colstarts (csc-colstarts matrix))
         (rows (csc-rows matrix))
         (values (csc-values matrix))
         (colperm (make-array m :element-type 'fixnum :initial-element 0))
         (icolperm (make-array m :element-type 'fixnum :initial-element +nan+))
         (rowperm (make-array m :element-type 'fixnum :initial-element 0))
         (irowperm (make-array m :element-type 'fixnum :initial-element +nan+))
         ;; the number of non-zero elements in each column
         (b-degs (make-array m :element-type 'fixnum :initial-element 0))
         (bt-degs (make-array m :element-type 'fixnum :initial-element 0))
         ;; HEAP keeps the column numbers in ascending order of degree.
         ;; Priority of col is M+1 iff B-DEGS[col] is zero;
         ;; Priority of col is M+2 iff col has no elements which are larger than +EPSNUM+.
         (heap (make-heap m))
         (tmp (make-array m :element-type 'fixnum :initial-element 0))
         (tmp2 (make-array m :element-type 'fixnum :initial-element +nan+)))
    (assert (= m (length basis)))
    (dotimes (i m)
      (let ((start (aref colstarts (aref basis i)))
            (end (aref colstarts (+ 1 (the fixnum (aref basis i))))))
        (declare ((mod #.array-dimension-limit) start end))
        (setf (aref b-degs i) (- end start))
        (loop for k from start below end
              do (incf (aref bt-degs (aref rows k))))))
    (let* ((estimated-nz (%power-of-two-ceiling
                          (ash (the fixnum (reduce #'+ b-degs)) -1)))
           (lower-colstarts (make-array (+ m 1) :element-type 'fixnum :initial-element 0))
           (lower-rows (make-array estimated-nz :element-type 'fixnum))
           (lower-values (make-array estimated-nz :element-type 'csc-float))
           (tupper-colstarts (make-array (+ m 1) :element-type 'fixnum :initial-element 0))
           (tupper-rows (make-array estimated-nz :element-type 'fixnum))
           (tupper-values (make-array estimated-nz :element-type 'csc-float))
           (diagu (make-array m :element-type 'csc-float))
           (b-indices (make-array m :element-type t))
           (b-values (make-array m :element-type t))
           (bt-indices (make-array m :element-type t))
           (bt-values (make-array m :element-type t))
           (rank m)
           (tag 1)
           (lower-nz 0)
           (upper-nz 0))
      (declare (ivec lower-rows tupper-rows)
               (fvec lower-values tupper-values)
               ((mod #.array-dimension-limit) rank tag lower-nz upper-nz))
      ;; initialize B and BT
      (dotimes (i m)
        (setf (aref b-indices i) (make-array (aref b-degs i) :element-type 'fixnum)
              (aref b-values i) (make-array (aref b-degs i) :element-type 'csc-float)
              (aref bt-indices i) (make-array (aref bt-degs i) :element-type 'fixnum)
              (aref bt-values i) (make-array (aref bt-degs i) :element-type 'csc-float)))
      (let ((bt-poses tmp))
        (dotimes (i m)
          (let ((bpos 0)
                (start (aref colstarts (aref basis i)))
                (end (aref colstarts (+ 1 (the fixnum (aref basis i))))))
            (declare ((mod #.array-dimension-limit) start end))
            (loop for k from start below end
                  for row = (aref rows k)
                  for bt-pos = (aref bt-poses row)
                  do (setf (aref (the ivec (aref b-indices i)) bpos) row
                           (aref (the fvec (aref b-values i)) bpos) (aref values k)
                           (aref (the ivec (aref bt-indices row)) bt-pos) i
                           (aref (the fvec (aref bt-values row)) bt-pos) (aref values k))
                     (incf (aref bt-poses row))
                     (incf bpos))))
        (fill bt-poses 0))
      ;; initialize heap
      (dotimes (j m)
        (heap-ensure-key heap j
                         (if (zerop (aref b-degs j)) (+ m 1) (aref b-degs j))))
      (block decomposition
        (dotimes (i m)
          (multiple-value-bind (pivot-col coldeg pivot-row rowdeg)
              (loop
                (multiple-value-bind (col priority) (heap-pop heap)
                  (when (> priority m)
                    ;; singular matrix
                    (when (= priority (+ m 1))
                      (assert (zerop (aref b-degs col))))
                    (setq rank i)
                    (return-from decomposition))
                  (let ((coldeg (aref b-degs col))
                        (rowdeg (+ m 1))
                        row)
                    ;; select row of minimal degree
                    (dotimes (k coldeg)
                      (when (and (< (aref bt-degs (aref (the ivec (aref b-indices col)) k))
                                    rowdeg)
                                 (> (abs (aref (the fvec (aref b-values col)) k))
                                    +epsnum+))
                        (setq row (aref (the ivec (aref b-indices col)) k)
                              rowdeg (aref bt-degs row))))
                    (when (< rowdeg (+ m 1))
                      (return (values col coldeg row rowdeg)))
                    ;; col has no elements which are sufficiently distant from zero
                    (heap-ensure-key heap col (+ m 2)))))
            (declare ((mod #.array-dimension-limit) pivot-col coldeg pivot-row rowdeg))
            (setf (aref colperm i) pivot-col
                  (aref icolperm pivot-col) i
                  (aref rowperm i) pivot-row
                  (aref irowperm pivot-row) i)
            ;; copy pivot column into LOWER and pivot row into TUPPER
            (setf (aref lower-colstarts (+ i 1))
                  (+ (aref lower-colstarts i) coldeg -1))
            (dotimes (k coldeg)
              (let ((row (aref (the ivec (aref b-indices pivot-col)) k))
                    (val (aref (the fvec (aref b-values pivot-col)) k)))
                (unless (= row pivot-row)
                  (vector-set* lower-rows lower-nz row)
                  (vector-set* lower-values lower-nz val)
                  (incf lower-nz))))
            (setf (aref tupper-colstarts (+ i 1))
                  (+ (aref tupper-colstarts i) rowdeg -1))
            (dotimes (k rowdeg)
              (let ((col (aref (the ivec (aref bt-indices pivot-row)) k))
                    (val (aref (the fvec (aref bt-values pivot-row)) k)))
                (if (= col pivot-col)
                    (setf (aref diagu i) val)
                    (progn
                      (vector-set* tupper-rows upper-nz col)
                      (vector-set* tupper-values upper-nz val)
                      (incf upper-nz)))))
            ;; remove pivot rows and element from B and BT
            (dotimes (k coldeg)
              (let ((row (aref (the ivec (aref b-indices pivot-col)) k)))
                (decf (aref bt-degs row))
                (let ((bt-pos 0))
                  (loop until (= pivot-col (aref (the ivec (aref bt-indices row)) bt-pos))
                        do (incf bt-pos))
                  (rotatef (aref (the ivec (aref bt-indices row)) bt-pos)
                           (aref (the ivec (aref bt-indices row)) (aref bt-degs row)))
                  (rotatef (aref (the fvec (aref bt-values row)) bt-pos)
                           (aref (the fvec (aref bt-values row)) (aref bt-degs row))))))
            (dotimes (k rowdeg)
              (let ((col (aref (the ivec (aref bt-indices pivot-row)) k)))
                (decf (aref b-degs col))
                (let ((b-pos 0))
                  (loop until (= pivot-row (aref (the ivec (aref b-indices col)) b-pos))
                        do (incf b-pos))
                  (rotatef (aref (the ivec (aref b-indices col)) b-pos)
                           (aref (the ivec (aref b-indices col)) (aref b-degs col)))
                  (rotatef (aref (the fvec (aref b-values col)) b-pos)
                           (aref (the fvec (aref b-values col)) (aref b-degs col))))))
            (setf (aref b-degs pivot-col) 0
                  (aref bt-degs pivot-row) 0)
            ;; update B and BT
            (loop
              with tags = tmp
              with bt-poses = tmp2
              for k from (aref lower-colstarts i) below (aref lower-colstarts (+ i 1))
              for row = (aref lower-rows k)
              do (dotimes (bt-pos (aref bt-degs row))
                   (setf (aref tags (aref (the ivec (aref bt-indices row)) bt-pos)) tag
                         (aref bt-poses (aref (the ivec (aref bt-indices row)) bt-pos)) bt-pos))
                 (loop
                   for kk from (aref tupper-colstarts i) below (aref tupper-colstarts (+ i 1))
                   for col = (aref tupper-rows kk)
                   for decr of-type csc-float = (/ (* (aref lower-values k)
                                                      (aref tupper-values kk))
                                                   (aref diagu i))
                   when (= (aref tags col) tag)
                   do (decf (aref (the fvec (aref bt-values row)) (aref bt-poses col))
                            decr)
                   else
                   do (let ((deg (aref bt-degs row)))
                        (vector-set* (the ivec (aref bt-indices row)) deg col)
                        (vector-set* (the fvec (aref bt-values row)) deg (- decr))
                        (incf (aref bt-degs row))))
                 (incf tag))
            (loop
              with tags = tmp
              with b-poses = tmp2
              for k from (aref tupper-colstarts i) below (aref tupper-colstarts (+ i 1))
              for col = (aref tupper-rows k)
              do (dotimes (b-pos (aref b-degs col))
                   (setf (aref tags (aref (the ivec (aref b-indices col)) b-pos)) tag
                         (aref b-poses (aref (the ivec (aref b-indices col)) b-pos)) b-pos))
                 (loop
                   for kk from (aref lower-colstarts i) below (aref lower-colstarts (+ i 1))
                   for row = (aref lower-rows kk)
                   for decr of-type csc-float = (/ (* (aref lower-values kk)
                                                      (aref tupper-values k))
                                                   (aref diagu i))
                   when (= (aref tags row) tag)
                   do (decf (aref (the fvec (aref b-values col)) (aref b-poses row))
                            decr)
                   else
                   do (let ((deg (aref b-degs col)))
                        (vector-set* (the ivec (aref b-indices col)) deg row)
                        (vector-set* (the fvec (aref b-values col)) deg (- decr))
                        (incf (aref b-degs col))))
                 (incf tag))
            (loop for k from (aref tupper-colstarts i) below (aref tupper-colstarts (+ i 1))
                  for col = (aref tupper-rows k)
                  do (heap-ensure-key heap col
                                      (if (zerop (aref b-degs col))
                                          (+ m 1)
                                          (aref b-degs col)))))))
      ;; fill dependent rows and cols
      (let ((end rank))
        (declare ((mod #.array-dimension-limit) end))
        (dotimes (col m)
          (when (= +nan+ (aref icolperm col))
            (setf (aref colperm end) col
                  (aref icolperm col) end
                  end (+ end 1)))))
      (let ((end rank))
        (declare ((mod #.array-dimension-limit) end))
        (dotimes (row m)
          (when (= +nan+ (aref irowperm row))
            (setf (aref rowperm end) row
                  (aref irowperm row) end
                  end (+ end 1)))))
      (loop for i from rank below m
            do (setf (aref lower-colstarts (+ i 1)) (aref lower-colstarts i)
                     (aref tupper-colstarts (+ i 1)) (aref tupper-colstarts i)
                     (aref diagu i) +zero+))
      (dotimes (k (aref lower-colstarts m))
        (setf (aref lower-rows k)
              (aref irowperm (aref lower-rows k))))
      (dotimes (k (aref tupper-colstarts m))
        (setf (aref tupper-rows k)
              (aref icolperm (aref tupper-rows k))))
      (dotimes (i m)
        (loop for k from (aref lower-colstarts i) below (aref lower-colstarts (+ i 1))
              do (setf (aref lower-values k)
                       (/ (aref lower-values k) (aref diagu i)))))
      (let* ((lower (make-csc m m lower-nz lower-colstarts lower-rows lower-values))
             (tupper (make-csc m m upper-nz tupper-colstarts tupper-rows tupper-values))
             (tlower (csc-transpose lower))
             (upper (csc-transpose tupper)))
        (make-lud :m m
                  :lower lower
                  :tlower tlower
                  :upper upper
                  :tupper tupper
                  :diagu diagu
                  :rank rank
                  :colperm colperm
                  :icolperm icolperm
                  :rowperm rowperm
                  :irowperm irowperm)))))

(defun dense-solve! (lud y)
  "Solves LUx = y and stores the solution to Y. The consequence is undefined
when it is infeasible."
  (declare (optimize (speed 3))
           (fvec y))
  (let* ((m (lud-m lud))
         (irowperm (lud-irowperm lud))
         (colperm (lud-colperm lud))
         (lower (lud-lower lud))
         (lower-colstarts (csc-colstarts lower))
         (lower-rows (csc-rows lower))
         (lower-values (csc-values lower))
         (tupper (lud-tupper lud))
         (tupper-colstarts (csc-colstarts tupper))
         (tupper-rows (csc-rows tupper))
         (tupper-values (csc-values tupper))
         (diagu (lud-diagu lud))
         (rank (lud-rank lud))
         (tmp (make-array m :element-type 'csc-float)))
    (dotimes (i m)
      (setf (aref tmp i) (aref y i)))
    (dotimes (i m)
      (setf (aref y (aref irowperm i)) (aref tmp i)))
    ;; y := L^(-1)y
    (dotimes (i rank)
      (let ((val (aref y i)))
        (loop for k from (aref lower-colstarts i) below (aref lower-colstarts (+ i 1))
              for row = (aref lower-rows k)
              do (decf (aref y row) (* (aref lower-values k) val)))))
    (loop for i from (- m 1) downto rank
          do (setf (aref y i) +zero+))
    (loop for i from (- rank 1) downto 0
          for val of-type csc-float = (aref y i)
          do (loop for k from (aref tupper-colstarts i) below (aref tupper-colstarts (+ i 1))
                   do (decf val (* (aref tupper-values k) (aref y (aref tupper-rows k)))))
             (setf (aref y i) (/ val (aref diagu i))))
    (dotimes (i m)
      (setf (aref tmp i) (aref y i)))
    (dotimes (i m)
      (setf (aref y (aref colperm i)) (aref tmp i)))
    y))

(defstruct (lud-eta (:constructor %make-lud-eta))
  (lud nil :type lud)
  (count 0 :type (mod #.array-dimension-limit))
  (nz 0 :type (mod #.array-dimension-limit))
  ;; This vector stores the leaving columns of each pivotting. Note that this
  ;; `column' doesn't mean the index of some variable but the column number of
  ;; the matrix B.
  (leaving-cols nil :type (simple-array fixnum (*)))
  (colstarts nil :type (simple-array fixnum (*)))
  (rows nil :type (simple-array fixnum (*)))
  (values nil :type (simple-array csc-float (*)))
  (cumtime 0 :type (unsigned-byte 64))
  (prev-cumtime 0 :type (unsigned-byte 64)))

(defun make-lud-eta (lud &key (initial-size 0) cumtime prev-cumtime)
  (declare ((or null (unsigned-byte 64)) cumtime prev-cumtime))
  (%make-lud-eta :lud lud
                 :leaving-cols (make-array 0 :element-type 'fixnum)
                 :colstarts (make-array 1 :element-type 'fixnum :initial-element 0)
                 :rows (make-array initial-size :element-type 'fixnum)
                 :values (make-array initial-size :element-type 'csc-float)
                 :cumtime (or cumtime 0)
                 :prev-cumtime (or prev-cumtime 0)))

(defun refactor (matrix basis)
  (let* ((start-time (read-tsc))
         (lud (lu-factor matrix basis))
         (timedelta (ldb (byte 64 0) (- (read-tsc) start-time))))
    (make-lud-eta lud :cumtime timedelta)))

(defun add-eta! (lud-eta leaving-col sparse-vector)
  (declare (optimize (speed 3)))
  (symbol-macrolet ((enz (lud-eta-nz lud-eta))
                    (ecount (lud-eta-count lud-eta))
                    (vector-nz (sparse-vector-nz sparse-vector))
                    (leaving-cols (lud-eta-leaving-cols lud-eta))
                    (colstarts (lud-eta-colstarts lud-eta))
                    (rows (lud-eta-rows lud-eta))
                    (values (lud-eta-values lud-eta)))
    (vector-set* leaving-cols ecount leaving-col)
    (loop for k from enz below (+ enz vector-nz)
          for pos across (sparse-vector-indices sparse-vector)
          for value of-type csc-float across (sparse-vector-values sparse-vector)
          do (vector-set* rows k pos)
             (vector-set* values k value))
    (incf ecount)
    (incf enz vector-nz)
    (vector-set* colstarts ecount enz)
    lud-eta))

(defmacro with-updating-time (lud-eta &body body)
  (let ((start-time (gensym "START"))
        (timedelta (gensym "DELTA"))
        (lude (gensym "LUDE")))
    `(let ((,start-time (read-tsc))
           (,lude ,lud-eta))
       (unwind-protect (progn ,@body)
         (let ((,timedelta (ldb (byte 64 0) (- (read-tsc) ,start-time))))
           (setf (lud-eta-cumtime ,lude)
                 (ldb (byte 64 0) (+ (lud-eta-cumtime ,lude) ,timedelta))))))))
;; temporary storage
(defconstant +initial-size+ 16)

(declaim (fvec *gauss-eta-values*)
         (ivec *gauss-eta-tags* *gauss-eta-rows*)
         ((integer 0 #.most-positive-fixnum) *gauss-eta-tag*))
(defparameter *gauss-eta-values* (make-array +initial-size+ :element-type 'csc-float))
(defparameter *gauss-eta-tags* (make-array +initial-size+ :element-type 'fixnum))
(defparameter *gauss-eta-tag* 1)
(defparameter *gauss-eta-rows* (make-array +initial-size+ :element-type 'fixnum))

(defmacro dbg (&rest forms)
  (declare (ignorable forms))
  #+swank (if (= (length forms) 1)
              `(format *error-output* "~A => ~A~%" ',(car forms) ,(car forms))
              `(format *error-output* "~A => ~A~%" ',forms `(,,@forms))))

(defun gauss-eta! (lud-eta xb)
  (declare (optimize (speed 3))
           (sparse-vector xb))
  (let* ((lud (lud-eta-lud lud-eta))
         (m (lud-m lud)))
    (extend-vectorf *gauss-eta-values* m)
    (extend-vectorf *gauss-eta-tags* m)
    (extend-vectorf *gauss-eta-rows* m))
  (with-updating-time lud-eta
    (let* ((vector-nz (sparse-vector-nz xb))
           (vector-values (sparse-vector-values xb))
           (vector-indices (sparse-vector-indices xb))
           (tmp-values *gauss-eta-values*)
           (tmp-tags *gauss-eta-tags*)
           (tmp-rows *gauss-eta-rows*)
           (tmp-end 0)
           (leaving-cols (lud-eta-leaving-cols lud-eta))
           (colstarts (lud-eta-colstarts lud-eta))
           (values (lud-eta-values lud-eta))
           (rows (lud-eta-rows lud-eta)))
      (declare (fvec tmp-values vector-values)
               (ivec tmp-rows tmp-tags vector-indices)
               ((mod #.array-dimension-limit) tmp-end))
      (when (zerop (lud-eta-count lud-eta))
        (return-from gauss-eta!))
      (labels ((add-to-tmp (nzpos value)
                 (setf (aref tmp-values nzpos) value
                       (aref tmp-tags nzpos) *gauss-eta-tag*
                       (aref tmp-rows tmp-end) nzpos)
                 (incf tmp-end)))
        (dotimes (k vector-nz)
          (add-to-tmp (aref vector-indices k) (aref vector-values k)))
        (dotimes (j (lud-eta-count lud-eta))
          (let ((leaving-col (aref leaving-cols j))
                leaving-col-k)
            (loop for k from (aref colstarts j) below (aref colstarts (+ j 1))
                  for row = (aref rows k)
                  unless (= (aref tmp-tags row) *gauss-eta-tag*)
                  do (add-to-tmp row +zero+)
                  when (= row leaving-col)
                  do (setq leaving-col-k k))
            (let ((coef (/ (aref tmp-values leaving-col) (aref values leaving-col-k))))
              (unless (zerop coef)
                (loop for k from (aref colstarts j) below leaving-col-k
                      for row = (aref rows k)
                      do (decf (aref tmp-values row) (* (aref values k) coef)))
                (setf (aref tmp-values leaving-col) coef)
                (loop for k from (+ leaving-col-k 1) below (aref colstarts (+ j 1))
                      for row = (aref rows k)
                      do (decf (aref tmp-values row) (* (aref values k) coef)))))))
        (let ((nz 0))
          (dotimes (k tmp-end)
            (let ((row (aref tmp-rows k)))
              (when (> (abs (aref tmp-values row)) +eps+)
                (vector-set* vector-values nz (aref tmp-values row))
                (vector-set* vector-indices nz row)
                (incf nz))))
          (setf (sparse-vector-values xb) vector-values
                (sparse-vector-indices xb) vector-indices
                (sparse-vector-nz xb) nz))
        (incf *gauss-eta-tag*)
        xb))))

(defun gauss-eta-transposed! (lud-eta xb)
  (declare (optimize (speed 3)))
  (let* ((lud (lud-eta-lud lud-eta))
         (m (lud-m lud)))
    (extend-vectorf *gauss-eta-values* m)
    (extend-vectorf *gauss-eta-tags* m))
  (with-updating-time lud-eta
    (let* ((vector-nz (sparse-vector-nz xb))
           (vector-values (sparse-vector-values xb))
           (vector-indices (sparse-vector-indices xb))
           (tmp-values *gauss-eta-values*)
           (tmp-tags *gauss-eta-tags*)
           (leaving-cols (lud-eta-leaving-cols lud-eta))
           (colstarts (lud-eta-colstarts lud-eta))
           (values (lud-eta-values lud-eta))
           (rows (lud-eta-rows lud-eta)))
      (declare (fvec tmp-values vector-values)
               (ivec tmp-tags vector-indices))
      (loop for j from (- (lud-eta-count lud-eta) 1) downto 0
            for leaving-col = (aref leaving-cols j)
            for leaving-col-k = nil
            do (dotimes (k vector-nz)
                 (let ((row (aref vector-indices k)))
                   (when (= row leaving-col)
                     (setq leaving-col-k k))
                   (setf (aref tmp-values row) (aref vector-values k)
                         (aref tmp-tags row) *gauss-eta-tag*)))
               (unless (= *gauss-eta-tag* (aref tmp-tags leaving-col))
                 (vector-set* vector-values vector-nz +zero+)
                 (vector-set* vector-indices vector-nz leaving-col)
                 (setq leaving-col-k vector-nz)
                 (incf vector-nz)
                 (setf (aref tmp-values leaving-col) +zero+
                       (aref tmp-tags leaving-col) *gauss-eta-tag*))
               (let ((tmp (aref vector-values leaving-col-k))
                     leaving-col-eta-k)
                 (declare (csc-float tmp))
                 (loop for k from (aref colstarts j) below (aref colstarts (+ j 1))
                       for row = (aref rows k)
                       when (= row leaving-col)
                       do (setq leaving-col-eta-k k)
                       when (and (= (aref tmp-tags row) *gauss-eta-tag*)
                                 (/= row leaving-col))
                       do (decf tmp (* (aref values k) (aref tmp-values row))))
                 (incf *gauss-eta-tag*)
                 (setf (aref vector-values leaving-col-k)
                       (/ tmp (aref values leaving-col-eta-k)))))
      (setf (sparse-vector-values xb) vector-values
            (sparse-vector-indices xb) vector-indices
            (sparse-vector-nz xb) vector-nz)
      xb)))

(declaim (fvec *sparse-solve-values*)
         (ivec *sparse-solve-tags* *sparse-solve-rows*)
         ((integer 0 #.most-positive-fixnum) *sparse-solve-tag*))
(defparameter *sparse-solve-values* (make-array +initial-size+ :element-type 'csc-float))
(defparameter *sparse-solve-tags* (make-array +initial-size+ :element-type 'fixnum))
(defparameter *sparse-solve-tag* 1)

(defun sparse-solve! (lud-eta y)
  (declare (optimize (speed 3)))
  (let* ((lud (lud-eta-lud lud-eta))
         (m (lud-m lud)))
    (extend-vectorf *sparse-solve-values* m)
    (extend-vectorf *sparse-solve-tags* m))
  (with-updating-time lud-eta
    (let* ((lud (lud-eta-lud lud-eta))
           (rank (lud-rank lud))
           (vector-nz (sparse-vector-nz y))
           (vector-indices (sparse-vector-indices y))
           (vector-values (sparse-vector-values y))
           (tree (make-iterset))
           ;; vector values sorted w.r.t. the order of LU decomposition
           (tmp-values *sparse-solve-values*)
           (tmp-tags *sparse-solve-tags*)
           (tag *sparse-solve-tag*))
      (declare (fvec tmp-values vector-values)
               (ivec tmp-tags vector-indices))
      (labels ((add-to-tmp (lu-pos value)
                 (setf (aref tmp-values lu-pos) value
                       (aref tmp-tags lu-pos) tag)
                 (iterset-insert tree lu-pos)))
        (let ((irowperm (lud-irowperm lud)))
          (dotimes (k vector-nz)
            (let ((lu-pos (aref irowperm (aref vector-indices k))))
              (add-to-tmp lu-pos (aref vector-values k)))))
        ;; y := L^(-1)y
        (let* ((lower (lud-lower lud))
               (lower-colstarts (csc-colstarts lower))
               (lower-rows (csc-rows lower))
               (lower-values (csc-values lower))
               (node (iterset-first tree)))
          (loop while (and node (< (node-key node) rank))
                for lu-pos = (node-key node)
                for beta of-type csc-float = (aref tmp-values lu-pos)
                for start = (aref lower-colstarts lu-pos)
                for end = (aref lower-colstarts (+ lu-pos 1))
                do (loop for k from start below end 
                         for row = (aref lower-rows k)
                         unless (= (aref tmp-tags row) tag)
                         do (add-to-tmp row +zero+)
                         do (decf (aref tmp-values row) (* (aref lower-values k) beta)))
                   (setq node (node-next node))))
        ;; fill free variable
        ;; y := U^(-1)y
        (let* ((upper (lud-upper lud))
               (upper-colstarts (csc-colstarts upper))
               (upper-rows (csc-rows upper))
               (upper-values (csc-values upper))
               (diagu (lud-diagu lud))
               (node (iterset-last tree)))
          (loop while (and node (>= (node-key node) rank))
                for lu-pos = (node-key node)
                do (setf (aref tmp-values lu-pos) +zero+)
                   (setq node (node-prev node)))
          (loop while node
                for lu-pos = (node-key node)
                for beta of-type csc-float = (/ (aref tmp-values lu-pos) (aref diagu lu-pos))
                for start = (aref upper-colstarts lu-pos)
                for end = (aref upper-colstarts (+ lu-pos 1))
                do (loop for k from start below end
                         for row = (aref upper-rows k)
                         unless (= (aref tmp-tags row) tag)
                         do (add-to-tmp row +zero+)
                         do (decf (aref tmp-values row) (* (aref upper-values k) beta)))
                   (setf (aref tmp-values lu-pos) beta)
                   (setq node (node-prev node)))))
      ;; update y
      (let ((nz 0)
            (node (iterset-first tree))
            (colperm (lud-colperm lud)))
        (declare ((mod #.array-dimension-limit) nz))
        (loop while node
              for lu-pos = (node-key node)
              when (> (abs (aref tmp-values lu-pos)) +eps+)
              do (vector-set* vector-values nz (aref tmp-values lu-pos))
                 (vector-set* vector-indices nz (aref colperm lu-pos))
                 (incf nz)
              do (setq node (node-next node)))
        (setf (sparse-vector-nz y) nz
              (sparse-vector-indices y) vector-indices
              (sparse-vector-values y) vector-values))
      (gauss-eta! lud-eta y)
      (incf *sparse-solve-tag*)
      y)))

(defun sparse-solve-transposed! (lud-eta y)
  (declare (optimize (speed 3)))
  (let* ((lud (lud-eta-lud lud-eta))
         (m (lud-m lud)))
    (extend-vectorf *sparse-solve-values* m)
    (extend-vectorf *sparse-solve-tags* m))
  (with-updating-time lud-eta
    (gauss-eta-transposed! lud-eta y)
    (let* ((lud (lud-eta-lud lud-eta))
           (rank (lud-rank lud))
           (vector-nz (sparse-vector-nz y))
           (vector-indices (sparse-vector-indices y))
           (vector-values (sparse-vector-values y))
           (tree (make-iterset))
           ;; vector values sorted w.r.t. the order of LU decomposition
           (tmp-values *sparse-solve-values*)
           (tmp-tags *sparse-solve-tags*)
           (tag *sparse-solve-tag*))
      (declare (fvec tmp-values vector-values)
               (ivec tmp-tags vector-indices))
      (labels ((add-to-tmp (lu-pos value)
                 (setf (aref tmp-values lu-pos) value
                       (aref tmp-tags lu-pos) tag)
                 (iterset-insert tree lu-pos)))
        (let ((icolperm (lud-icolperm lud)))
          (dotimes (k vector-nz)
            (let ((lu-pos (aref icolperm (aref vector-indices k))))
              (add-to-tmp lu-pos (aref vector-values k)))))
        ;; y := U^(-T)y
        (let* ((tupper (lud-tupper lud))
               (tupper-colstarts (csc-colstarts tupper))
               (tupper-rows (csc-rows tupper))
               (tupper-values (csc-values tupper))
               (diagu (lud-diagu lud))
               (node (iterset-first tree)))
          (loop while (and node (< (node-key node) rank))
                for lu-pos = (node-key node)
                for beta of-type csc-float = (/ (aref tmp-values lu-pos) (aref diagu lu-pos))
                for start = (aref tupper-colstarts lu-pos)
                for end = (aref tupper-colstarts (+ 1 lu-pos))
                do (loop for k from start below end 
                         for row = (aref tupper-rows k)
                         unless (= (aref tmp-tags row) tag)
                         do (add-to-tmp row +zero+)
                         do (decf (aref tmp-values row) (* (aref tupper-values k) beta)))
                   (setf (aref tmp-values lu-pos) beta)
                   (setq node (node-next node))))
        ;; fill free varaiable
        ;; y := L^(-T)y
        (let* ((tlower (lud-tlower lud))
               (tlower-colstarts (csc-colstarts tlower))
               (tlower-rows (csc-rows tlower))
               (tlower-values (csc-values tlower))
               (node (iterset-last tree)))
          (loop while (and node (>= (node-key node) rank))
                for lu-pos = (node-key node)
                do (setf (aref tmp-values lu-pos) +zero+)
                   (setq node (node-prev node)))
          (loop while node
                for lu-pos = (node-key node)
                for beta of-type csc-float = (aref tmp-values lu-pos)
                for start = (aref tlower-colstarts lu-pos)
                for end = (aref tlower-colstarts (+ lu-pos 1))
                do (loop for k from start below end
                         for row = (aref tlower-rows k)
                         unless (= (aref tmp-tags row) tag)
                         do (add-to-tmp row +zero+)
                         do (decf (aref tmp-values row) (* (aref tlower-values k) beta)))
                   (setq node (node-prev node)))))
      ;; update y
      (let ((nz 0)
            (node (iterset-first tree))
            (rowperm (lud-rowperm lud)))
        (declare ((mod #.array-dimension-limit) nz))
        (loop while node
              for lu-pos = (node-key node)
              when (> (abs (aref tmp-values lu-pos)) +eps+)
              do (vector-set* vector-values nz (aref tmp-values lu-pos))
                 (vector-set* vector-indices nz (aref rowperm lu-pos))
                 (incf nz)
              do (setq node (node-next node)))
        (setf (sparse-vector-nz y) nz
              (sparse-vector-indices y) vector-indices
              (sparse-vector-values y) vector-values))
      (incf *sparse-solve-tag*)
      ;; #>y
      y)))

(declaim ((integer 0 #.most-positive-fixnum) *refactor-threshold*))
(defparameter *refactor-threshold* 200)
(defparameter *refactor-by-time* t)

(defun refactor-p (lud-eta leaving-col)
  (declare (optimize (speed 3))
           ((mod #.array-dimension-limit) leaving-col))
  (let ((count (lud-eta-count lud-eta))
        (colstarts (lud-eta-colstarts lud-eta))
        (rows (lud-eta-rows lud-eta)))
    (assert (> count 0))
    (prog1 (or (>= count *refactor-threshold*)
               ;; leaving column vanishes at the last E
               (loop for k from (aref colstarts (- count 1)) below (aref colstarts count)
                     never (= (aref rows k) leaving-col))
               (and *refactor-by-time*
                    (>= count 2)
                    (>= (floor (lud-eta-cumtime lud-eta) count)
                        (floor (lud-eta-prev-cumtime lud-eta) (- count 1)))))
      (setf (lud-eta-prev-cumtime lud-eta)
            (lud-eta-cumtime lud-eta)))))

;; (defparameter *mat56* #a((5 6) double-float
;;                          (2d0 0d0 4d0 0d0 -2d0 1d0)
;;                          (3d0 1d0 0d0 1d0 0d0 2d0)
;;                          (-1d0 0d0 -1d0 0d0 -2d0 3d0)
;;                          (0d0 -1d0 0d0 0d0 -6d0 0d0)
;;                          (0d0 0d0 1d0 0d0 4d0 0d0)))

;; (defparameter *new-mat* #a((5 6) double-float
;;                            (2d0 0d0 4d0 0d0 -2d0 5d0)
;;                            (3d0 1d0 0d0 1d0 0d0 0d0)
;;                            (-1d0 0d0 -1d0 0d0 -2d0 0d0)
;;                            (0d0 -1d0 0d0 0d0 -6d0 0d0)
;;                            (0d0 0d0 1d0 0d0 4d0 -1d0)))

;; (defparameter *mat1* #a((5 5) double-float
;;                         (2d0 0d0 4d0 0d0 -2d0)
;;                         (3d0 1d0 0d0 1d0 0d0)
;;                         (-1d0 0d0 -1d0 0d0 -2d0)
;;                         (0d0 -1d0 0d0 0d0 -6d0)
;;                         (0d0 0d0 1d0 0d0 4d0)))

;; (defparameter *mat2* #a((5 5) double-float
;;                         (2d0 0d0 4d0 0d0 1d0)
;;                         (3d0 1d0 0d0 1d0 2d0)
;;                         (-1d0 0d0 -1d0 0d0 3d0)
;;                         (0d0 -1d0 0d0 0d0 0d0)
;;                         (0d0 0d0 1d0 0d0 0d0)))

;; #a((5) double-float 7d0 -2d0 0d0 3d0 0d0)
;; #(-1.0d0 -0.0d0 2.0d0 1.0d0 -0.5d0) mat1
;; #(3.0d0 -3.0d0 0.0d0 -10.0d0 1.0d0) mat2

;; (let ((lude (make-lud-eta (lu-factor (make-csc-from-array cp/lu-decomposition::*new-mat*)
;;                                      #(0 1 2 3 4)))))
;;   (add-eta! lude 2 (make-sparse-vector-from #(-1d0 0d0 2d0 1d0 -0.5d0)))
;;   (sparse-solve! lude (make-sparse-vector-from #(5d0 0d0 0d0 0d0 -1d0))))
