(defpackage :cp/lu-decomposition
  (:use :cl :cp/csc #:cp/movable-binary-heap)
  (:import-from :cp/csc #:csc-float)
  (:export #:lu-factor #:lud-lower #:lud-upper #:lud-tlower #:lud-tupper #:lud-diagu
           #:lud-rank #:lud-colperm #:lud-icolperm #:lud-rowperm #:lud-irowperm #:lud-m
           #:make-lud-eta)
  (:documentation "Reference:
Robert J. Vanderbei. Linear Programming: Foundations and Extensions. 5th edition."))
(in-package :cp/lu-decomposition)

(defconstant +eps+ (coerce 1d-14 'csc-float))
(defconstant +epsnum+ (coerce 1d-9 'csc-float))

(declaim (inline %power-of-two-ceiling))
(defun %power-of-two-ceiling (x)
  (ash 1 (integer-length (- x 1))))

(defstruct (lud-base (:conc-name lud-))
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

(defstruct (lud-eta (:constructor %make-lud-eta))
  (lud nil :type lud-base)
  ;; eta-file
  (eta-iter 0 :type (mod #.array-dimension-limit))
  (nz 0 :type (mod #.array-dimension-limit))
  (leaving-cols nil :type (simple-array fixnum (*)))
  (colstarts nil :type (simple-array fixnum (*)))
  (rows nil :type (simple-array fixnum (*)))
  (values nil :type (simple-array csc-float (*))))

(defun make-lud-eta (lud &optional (initial-size 0))
  (%make-lud-eta :lud lud
                 :leaving-cols (make-array 0 :element-type 'fixnum)
                 :colstarts (make-array 1 :element-type 'fixnum :initial-element 0)
                 :rows (make-array initial-size :element-type 'fixnum)
                 :values (make-array initial-size :element-type 'csc-float)))

(defun extend-vector (vector size)
  (declare ((mod #.array-dimension-limit) size)
           (vector vector))
  (let ((new-size (%power-of-two-ceiling (max size 1))))
    (if (< (length vector) new-size)
        (adjust-array vector new-size)
        vector)))

(defmacro vector-set* (vector index new-element)
  (let ((i (gensym))
        (elm (gensym)))
    `(let ((,i ,index)
           (,elm ,new-element))
       (when (>= ,i (length ,vector))
         (setf ,vector (extend-vector ,vector
                                      (the (mod #.array-dimension-limit)
                                           (* 2 (length ,vector))))))
       (setf (aref ,vector ,i) ,elm))))

#+swank (set-dispatch-macro-character #\# #\> #'cl-debug-print:debug-print-reader)

(defparameter *mat* #a((5 5) double-float
                       (2d0 0d0 4d0 0d0 -2d0)
                       (3d0 1d0 0d0 1d0 0d0)
                       (-1d0 0d0 -1d0 0d0 -2d0)
                       (0d0 -1d0 0d0 0d0 -6d0)
                       (0d0 0d0 1d0 0d0 4d0)))
(defparameter *mat0* #a((5 5) double-float
                        (0d0 0d0 4d0 0d0 -2d0)
                        (0d0 1d0 0d0 1d0 0d0)
                        (0d0 0d0 -1d0 0d0 -2d0)
                        (0d0 -1d0 0d0 0d0 -6d0)
                        (0d0 0d0 1d0 0d0 4d0)))

(deftype ivec () '(simple-array fixnum (*)))
(deftype fvec () '(simple-array csc-float (*)))

(defconstant +nan+ -1)

(defun lu-factor (matrix basis)
  (declare (vector basis))
  ;; B: submatrix of MATRIX w.r.t. BASIS
  ;; BT: transposition of B
  (declare (optimize (speed 3)))
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
    (let* ((estimated-nz (max 1 (ash (the fixnum (reduce #'+ b-degs)) -1)))
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
                     (aref diagu i) 0d0))
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
        (make-lud-base :m m
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
