(defpackage :cp/csc
  (:use :cl)
  (:export
   #:coo #:coo-m #:coo-n #:make-coo #:coo-to-array #:coo-insert!
   #:coo-rows #:coo-cols #:coo-values #:coo-nz
   #:csc #:make-csc #:csc-to-array #:make-csc-from-array #:make-csc-from-coo
   #:csc-gemv #:csc-gemv-with-basis #:csc-transpose #:csc-m #:csc-n #:csc-nz
   #:csc-colstarts #:csc-rows #:csc-values
   #:sparse-vector #:make-sparse-vector #:make-sparse-vector-from
   #:make-array-from-sparse-vector
   #:sparse-vector-nz #:sparse-vector-values #:sparse-vector-indices)
  (:documentation "Provides some representations of a sparse matrix."))
(in-package :cp/csc)

(deftype csc-float () 'rational)
(defconstant +zero+ (coerce 0 'csc-float))
(defconstant +one+ (coerce 1 'csc-float))

(deftype index () '(mod #.array-dimension-limit))

(defstruct (coo (:constructor %make-coo (m n nz rows cols values))
                (:copier nil)
                (:predicate nil))
  "Stores a sparse matrix with coordinate list representation (aka COO
format). Note that M and N are automatically increased when you COO-INSERT! to
an out-of-bounds position, but you cannot decrease them."
  (m nil :type index)
  (n nil :type index)
  (nz nil :type index)
  (rows nil :type (simple-array fixnum (*)))
  (cols nil :type (simple-array fixnum (*)))
  (values nil :type (simple-array csc-float (*))))

(defun make-coo (m n &optional (initial-size 4))
  (declare ((integer 1 (#.array-dimension-limit)) initial-size))
  (let ((initial-size (max 1 initial-size)))
    (%make-coo m n 0
               (make-array initial-size :element-type 'fixnum)
               (make-array initial-size :element-type 'fixnum)
               (make-array initial-size :element-type 'csc-float))))

(defun coo-to-array (coo)
  "Makes a 2-dimensional array from a COO."
  (declare (optimize (speed 3)))
  (let* ((m (coo-m coo))
         (n (coo-n coo))
         (rows (coo-rows coo))
         (cols (coo-cols coo))
         (values (coo-values coo))
         (res (make-array (list m n) :element-type 'csc-float :initial-element +zero+)))
    (dotimes (pos (coo-nz coo))
      (setf (aref res (aref rows pos) (aref cols pos))
            (aref values pos)))
    res))

(define-modify-macro maxf (new-value) max)

(defun coo-insert! (coo row col value &optional fixed-size)
  "Destructively executes an assignment operation: COO[ROW][COL] := VALUE. If
FIXED-SIZE is NIL, the height and width of COO are automatically adjusted when
ROW or COL are out of bounds."
  (declare (optimize (speed 3))
           (index row col))
  (if fixed-size
      (assert (and (< row (coo-m coo))
                   (< col (coo-n coo))))
      (progn
        (maxf (coo-m coo) (+ 1 row))
        (maxf (coo-n coo) (+ 1 col))))
  (symbol-macrolet ((rows (coo-rows coo))
                    (cols (coo-cols coo))
                    (values (coo-values coo))
                    (nz (coo-nz coo)))
    (when (= nz (length rows))
      (let ((new-size (max 1 (the index (* 2 nz)))))
        (setq rows (adjust-array rows new-size)
              cols (adjust-array cols new-size)
              values (adjust-array values new-size))))
    (setf (aref rows nz) row
          (aref cols nz) col
          (aref values nz) value
          nz (+ nz 1))
    coo))

(defstruct (csc (:constructor make-csc (m n nz colstarts rows values))
                (:copier nil)
                (:predicate nil))
  "Stores a sparse matrix with compressed sparse column representation (aka CSC
format). Note that you can increase M after construction, but cannot decrease it
or change N."
  (m nil :type index)
  (n nil :type index)
  (nz nil :type index)
  (colstarts nil :type (simple-array fixnum (*)))
  (rows nil :type (simple-array fixnum (*)))
  (values nil :type (simple-array csc-float (*))))

(defun csc-to-array (csc &optional rowperm colperm)
  "Makes a 2-dimensional array from a CSC."
  (declare (optimize (speed 3))
           ((or null vector) rowperm colperm))
  (let* ((m (csc-m csc))
         (n (csc-n csc))
         (res (make-array (list m n) :element-type 'csc-float :initial-element +zero+))
         (colstarts (csc-colstarts csc))
         (rows (csc-rows csc))
         (values (csc-values csc)))
    (dotimes (col n)
      (let ((dest-col (if colperm (aref colperm col) col)))
        (loop for i from (aref colstarts col) below (aref colstarts (+ col 1))
              for row = (aref rows i)
              for dest-row = (if rowperm (aref rowperm row) row)
              for value = (aref values i)
              do (setf (aref res dest-row dest-col) value))))
    res))

(defun make-csc-from-array (array)
  "Makes a CSC from a 2-dimensional array."
  (declare (optimize (speed 3))
           ((array * (* *)) array))
  (destructuring-bind (m n) (array-dimensions array)
    (declare (index m n))
    (let* ((colstarts (make-array (+ n 1) :element-type 'fixnum))
           (nz (count +zero+ (sb-ext:array-storage-vector array) :test-not #'=))
           (rows (make-array nz :element-type 'fixnum))
           (values (make-array nz :element-type 'csc-float))
           (end 0))
      (declare (index end))
      (dotimes (col n)
        (setf (aref colstarts col) end)
        (dotimes (row m)
          (let ((value (aref array row col)))
            (unless (zerop value)
              (setf (aref rows end) row
                    (aref values end) value)
              (incf end)))))
      (setf (aref colstarts n) end)
      (make-csc m n nz colstarts rows values))))

(declaim (inline make-csc-from-coo))
(defun make-csc-from-coo (coo)
  "Makes a CSC from a COO. Note that the returned CSC contains zero when COO
contains it."
  (declare (optimize (speed 3))
           (inline stable-sort))
  (let* ((m (coo-m coo))
         (n (coo-n coo))
         (rows (coo-rows coo))
         (cols (coo-cols coo))
         (values (coo-values coo))
         (indices (let ((tmp (make-array (coo-nz coo) :element-type 'fixnum)))
                    (dotimes (i (length tmp))
                      (setf (aref tmp i) i))
                    (stable-sort tmp (lambda (i1 i2)
                                       (if (= (aref cols i1) (aref cols i2))
                                           (< (aref rows i1) (aref rows i2))
                                           (< (aref cols i1) (aref cols i2)))))))
         (nz 0))
    (declare ((simple-array fixnum (*)) indices)
             (index nz))
    ;; drop duplicate elements
    (dotimes (i* (length indices))
      (let ((i (aref indices i*)))
        (if (and (> i* 0)
                 (let ((prev-i (aref indices (- i* 1))))
                   (and (= (aref rows i) (aref rows prev-i))
                        (= (aref cols i) (aref cols prev-i)))))
            (setf (aref indices (- nz 1)) i)
            (setf (aref indices nz) i
                  nz (+ nz 1)))))
    (let ((colstarts (make-array (+ n 1) :element-type 'fixnum))
          (csc-rows (make-array nz :element-type 'fixnum))
          (csc-values (make-array nz :element-type 'csc-float))
          (end 0)
          (prev-col -1))
      (declare (index end)
               ((integer -1 (#.array-dimension-limit)) prev-col))
      (dotimes (i* nz)
        (let* ((i (aref indices i*))
               (row (aref rows i))
               (col (aref cols i))
               (value (aref values i)))
          (loop for j from col above prev-col
                do (setf (aref colstarts j) end))
          (setf (aref csc-rows end) row
                (aref csc-values end) value)
          (incf end)
          (setq prev-col col)))
      (loop for j from n above prev-col
            do (setf (aref colstarts j) end))
      (make-csc m n nz colstarts csc-rows csc-values))))

(declaim (inline csc-gemv-with-basis))
(defun csc-gemv-with-basis (csc vector basis)
  "Multiplies CSC and VECTOR and returns a resultant vector."
  (declare (vector vector basis))
  (let* ((m (csc-m csc))
         (colstarts (csc-colstarts csc))
         (rows (csc-rows csc))
         (values (csc-values csc))
         (res (make-array m :element-type 'csc-float :initial-element +zero+)))
    (dotimes (i m)
      (let ((bi (aref basis i)))
        (loop for k from (aref colstarts bi) below (aref colstarts (+ bi 1))
              do (incf (aref res (aref rows k))
                       (* (aref values k) (coerce (aref vector i) 'csc-float))))))
    res))

(declaim (inline csc-gemv))
(defun csc-gemv (csc vector)
  "Multiplies CSC and VECTOR and returns a resultant vector."
  (declare (vector vector))
  (let* ((m (csc-m csc))
         (n (length vector))
         (colstarts (csc-colstarts csc))
         (rows (csc-rows csc))
         (values (csc-values csc))
         (res (make-array m :element-type 'csc-float :initial-element +zero+)))
    (dotimes (j n)
      (loop for k from (aref colstarts j) below (aref colstarts (+ j 1))
            do (incf (aref res (aref rows k))
                     (* (aref values k) (coerce (aref vector j) 'csc-float)))))
    res))

(defun csc-transpose (csc)
  "Returns the transposed matrix of CSC. This function is non-destructive."
  (declare (optimize (speed 3)))
  (let* ((m (csc-m csc))
         (n (csc-n csc))
         (nz (csc-nz csc))
         (colstarts (csc-colstarts csc))
         (rows (csc-rows csc))
         (values (csc-values csc))
         (tmp (make-array m :element-type 'fixnum :initial-element 0))
         (new-colstarts (make-array (+ m 1) :element-type 'fixnum :initial-element 0))
         (new-rows (make-array nz :element-type 'fixnum :initial-element 0))
         (new-values (make-array nz :element-type 'csc-float :initial-element +zero+)))
    (dotimes (k nz)
      (let ((row (aref rows k)))
        (incf (aref tmp row))))
    ;; TMP[ROW] == the number of the elements in ROW
    (dotimes (i m)
      (setf (aref new-colstarts (+ i 1))
            (+ (aref new-colstarts i) (aref tmp i))))
    (fill tmp 0)
    (dotimes (j n)
      (loop for k from (aref colstarts j) below (aref colstarts (+ j 1))
            for row = (aref rows k)
            for new-pos of-type index = (+ (aref new-colstarts row) (aref tmp row))
            do (incf (aref tmp row))
               (setf (aref new-rows new-pos) j
                     (aref new-values new-pos) (aref values k))))
    (make-csc n m nz new-colstarts new-rows new-values)))

;; TODO: Should we store the intended dimension of vector?
(defstruct (sparse-vector (:constructor %make-sparse-vector (nz values indices)))
  (nz nil :type index)
  (values nil :type (simple-array csc-float (*)))
  (indices nil :type (simple-array fixnum (*))))

(defun make-sparse-vector (size)
  (%make-sparse-vector
   0
   (make-array size :element-type 'csc-float)
   (make-array size :element-type 'fixnum)))

(defun make-sparse-vector-from (vector)
  (declare (vector vector))
  (let* ((nz (count +zero+ vector :test-not #'=))
         (values (make-array nz :element-type 'csc-float))
         (indices (make-array nz :element-type 'fixnum))
         (end 0))
    (declare (index end))
    (dotimes (i (length vector))
      (unless (zerop (aref vector i))
        (setf (aref values end) (aref vector i)
              (aref indices end) i)
        (incf end)))
    (%make-sparse-vector nz values indices)))

(defun make-array-from-sparse-vector (sparse-vector)
  (let* ((nz (sparse-vector-nz sparse-vector))
         (indices (sparse-vector-indices sparse-vector))
         (values (sparse-vector-values sparse-vector))
         (m (if (zerop (length indices))
                0
                (+ 1 (reduce #'max indices))))
         (res (make-array m :element-type 'csc-float :initial-element +zero+)))
    (dotimes (i nz)
      (let ((index (aref indices i))
            (value (aref values i)))
        (setf (aref res index) value)))
    res))
