;;; unfinished

;;;
;;; Succinct bit vector
;;;

;; REVIEW: Is it really better to use the typical three-layer succint bit vector
;; in competitive programming? It may be efficient to use a two-layer (probably
;; not succint) bit vector preserving the original vector and the cumulative sum
;; per 64-bit word.

(defconstant +chunk-width+ (* 64 16))
;; This constant cannot be changed as the current implementation depends on the
;; assumption: +BLOCK-WIDTH+ is equal to the word size.
(defconstant +block-width+ 64)
(defconstant +block-number+ (floor +chunk-width+ +block-width+))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (assert (zerop (mod +chunk-width+ +block-width+)))
  (assert (= sb-vm:n-word-bits 64)))

(defstruct (succinct-bit-vector (:constructor %make-sucbv (storage chunks blocks))
                                (:conc-name sucbv-)
                                (:copier nil))
  (storage nil :type simple-bit-vector)
  (chunks nil :type (simple-array (integer 0 #.most-positive-fixnum) (*)))
  (blocks nil :type (simple-array (unsigned-byte 16) (* *))))

(defun make-sucbv! (vector)
  "The consequence is undefined when VECTOR is modified after a succinct bit
vector is created."
  (declare (optimize (speed 3)))
  (check-type vector simple-bit-vector)
  (let* ((vector (if (zerop (mod (length vector) +chunk-width+))
                     vector
                     (adjust-array vector
                                   (* +chunk-width+ (ceiling (length vector) +chunk-width+))
                                   :initial-element 0)))
         (len (length vector))
         (chunk-count (floor len +chunk-width+))
         (chunks (make-array (+ 1 chunk-count)
                             :element-type '(integer 0 #.most-positive-fixnum)
                             :initial-element 0))
         (blocks (make-array (list (+ 1 chunk-count) +block-number+)
                             :element-type '(unsigned-byte 16)
                             :initial-element 0))
         (sum 0))
    (declare (simple-bit-vector vector)
             ((integer 0 #.most-positive-fixnum) sum))
    (dotimes (i chunk-count)
      (setf (aref chunks i) sum)
      (let ((block-sum 0))
        (declare ((integer 0 #.most-positive-fixnum) block-sum))
        (dotimes (j +block-number+)
          (setf (aref blocks i j) block-sum)
          (incf block-sum
                (logcount (sb-kernel:%vector-raw-bits vector (+ (* i +block-number+) j)))))
        (incf sum block-sum)))
    (setf (aref chunks chunk-count) sum)
    (%make-sucbv vector chunks blocks)))

(declaim (inline sucbv-ref))
(defun sucbv-ref (sucbv index)
  (sbit (sucbv-storage sucbv) index))

;; NOTE: No error handling.
(declaim (ftype (function * (values (integer 0 #.most-positive-fixnum) &optional)) sucbv-rank))
(defun sucbv-rank (sucbv end)
  "Counts the number of 1's in the range [0, END)."
  (declare (optimize (speed 3))
           ((integer 0 #.most-positive-fixnum) end))
  (let ((storage (sucbv-storage sucbv))
        (chunks (sucbv-chunks sucbv))
        (blocks (sucbv-blocks sucbv)))
    (multiple-value-bind (cpos crem) (floor end +chunk-width+)
      (multiple-value-bind (bpos brem) (floor crem +block-width+)
        (let ((csum (aref chunks cpos))
              (bsum (aref blocks cpos bpos))
              (wordpos (floor end 64)))
          (+ csum
             bsum
             (if (zerop brem) ; avoid out-of-bounds access
                 0
                 (logcount (ldb (byte brem 0)
                                (sb-kernel:%vector-raw-bits storage wordpos))))))))))

(declaim (ftype (function * (values (integer 0 #.most-positive-fixnum) &optional)) sucbv-count))
(defun sucbv-count (sucbv value end)
  "Counts the number of VALUEs in the range [0, END)"
  (declare (optimize (speed 3))
           (bit value)
           ((integer 0 #.most-positive-fixnum) end))
  (let ((count1 (sucbv-rank sucbv end)))
    (if (= value 1)
        count1
        (- end count1))))

(defun sucbv-select (sucbv num)
  "Detects the position of (1-based) NUM-th 1 in SUCBV. (SUCBV-SELECT 0) always
returns 0."
  (declare (optimize (speed 3))
           ((integer 0 #.most-positive-fixnum) num))
  (let* ((storage (sucbv-storage sucbv))
         (chunks (sucbv-chunks sucbv))
         (blocks (sucbv-blocks sucbv))
         (chunk-size (length chunks)))
    (unless (<= num (aref chunks (- chunk-size 1)))
      ;; FIXME: introduce condition class
      (error "~&There aren't ~W 1's in ~W" num sucbv))
    (labels ((chunk-bisect (ok ng)
               (declare ((unsigned-byte 32) ok ng))
               (if (<= (- ng ok) 1)
                   ok
                   (let ((mid (ash (+ ok ng) -1)))
                     (if (<= num (aref chunks mid))
                         (chunk-bisect ok mid)
                         (chunk-bisect mid ng))))))
      (let* ((chunk-idx (chunk-bisect 0 chunk-size))
             (num (- num (aref chunks chunk-idx))))
        (labels ((block-bisect (ok ng)
                   (declare ((unsigned-byte 32) ok ng))
                   (if (<= (- ng ok) 1)
                       ok
                       (let ((mid (ash (+ ok ng) -1)))
                         (if (<= num (aref blocks chunk-idx mid))
                             (block-bisect ok mid)
                             (block-bisect mid ng))))))
          (let* ((block-idx (block-bisect 0 +block-number+))
                 (num (- num (aref blocks chunk-idx block-idx)))
                 (word-pos (+ block-idx (* chunk-idx +block-number+)))
                 (word (sb-kernel:%vector-raw-bits storage word-pos)))
            (labels ((pos-bisect (ok ng)
                       (declare ((integer 0 64) ok ng))
                       (if (<= (- ng ok) 1)
                           ok
                           (let ((mid (ash (+ ok ng) -1)))
                             (if (<= num (logcount (ldb (byte mid 0) word)))
                                 (pos-bisect ok mid)
                                 (pos-bisect mid ng))))))
              (let ((pos (pos-bisect 0 64)))
                (+ (* 64 word-pos) pos)))))))))

;;;
;;; Wavelet matrix
;;;

(deftype wavelet-integer () '(integer 0 #.most-positive-fixnum))

(defstruct (wavelet-matrix (:constructor %make-wavelet-matrix
                               (length data zeros
                                &aux (depth (array-dimension data 0))))
                           (:copier nil)
                           (:conc-name wavelet-))
  (depth 0 :type (integer 1 #.most-positive-fixnum))
  (length 0 :type (integer 0 #.most-positive-fixnum))
  (data nil :type (simple-array succinct-bit-vector (*)))
  (zeros nil :type (simple-array (integer 0 #.most-positive-fixnum) (*))))

#+sbcl
(eval-when (:compile-toplevel :load-toplevel :execute)
  (sb-c:defknown make-wavelet ((integer 1 #.most-positive-fixnum) vector)
      wavelet-matrix (sb-c:flushable sb-c:unsafely-flushable)))

;; TODO: add deftransform for better type derivation
(defun make-wavelet (bit-depth vector)
  (declare ((integer 1 #.most-positive-fixnum) bit-depth))
  (let* ((len (length vector))
         (fitted-len (* +chunk-width+ (ceiling len +chunk-width+)))
         (data (make-array bit-depth :element-type 'succinct-bit-vector))
         (zeros (make-array bit-depth :element-type '(integer 0 #.most-positive-fixnum)))
         (tmp (copy-seq vector))
         (lefts (make-array len :element-type (array-element-type vector)))
         (rights (make-array len :element-type (array-element-type vector)))
         (bits (make-array fitted-len :element-type 'bit)))
    (declare ((integer 0 #.most-positive-fixnum) len fitted-len))
    (loop for d from (- bit-depth 1) downto 0
          do (let ((lpos 0)
                   (rpos 0))
               (declare ((integer 0 #.most-positive-fixnum) lpos rpos))
               (dotimes (i len)
                 (let ((bit (logand 1 (ash (aref tmp i) (- d)))))
                   (if (zerop bit)
                       (setf (aref lefts lpos) (aref tmp i)
                             lpos (+ lpos 1))
                       (setf (aref rights rpos) (aref tmp i)
                             rpos (+ rpos 1)))
                   (setf (aref bits i) bit)))
               (setf (aref data d) (make-sucbv! (copy-seq bits))
                     (aref zeros d) lpos)
               (rotatef lefts tmp)
               (replace tmp rights :start1 lpos :end2 rpos)))
    (%make-wavelet-matrix len data zeros)))

(define-condition invalid-wavelet-index-error (type-error)
  ((wavelet :initarg :wavelet :reader invalid-wavelet-index-error-wavelet)
   (index :initarg :index :reader invalid-wavelet-index-error-index))
  (:report
   (lambda (condition stream)
     (let ((index (invalid-wavelet-index-error-index condition)))
       (if (consp index)
           (format stream "Invalid range [~W, ~W) for wavelet-matrix ~W."
                   (car index)
                   (cdr index)
                   (invalid-wavelet-index-error-wavelet condition))
           (format stream "Invalid index ~W for wavelet-matrix ~W."
                   index
                   (invalid-wavelet-index-error-wavelet condition)))))))

(defun wavelet-ref (wmatrix index)
  (declare (optimize (speed 3))
           ((integer 0 #.most-positive-fixnum) index))
  (let ((depth (wavelet-depth wmatrix))
        (data (wavelet-data wmatrix))
        (zeros (wavelet-zeros wmatrix))
        (res 0))
    (declare (wavelet-integer res))
    (when (>= index (wavelet-length wmatrix))
      (error 'invalid-wavelet-index-error :index index :wavelet wmatrix))
    (loop for d from (- depth 1) downto 0
          for sbv = (aref data d)
          for bit = (sucbv-ref sbv index)
          do (setq res (logior bit (ash res 1))
                   index (+ (sucbv-count sbv bit index)
                            (* bit (aref zeros d)))))
    res))

(defun wavelet-count (wmatrix value l r)
  "Returns the number of VALUE in [L, R)"
  (declare (wavelet-integer value)
           ((integer 0 #.most-positive-fixnum) l r))
  (let ((depth (wavelet-depth wmatrix))
        (data (wavelet-data wmatrix))
        (zeros (wavelet-zeros wmatrix)))
    (unless (<= l r (wavelet-length wmatrix))
      (error 'invalid-wavelet-index-error :index (cons l r) :wavelet wmatrix))
    (loop for d from (- depth 1) downto 0
          for bit = (logand 1 (ash value (- d)))
          do (setq l (+ (sucbv-count (aref data d) bit l)
                        (* bit (aref zeros d)))
                   r (+ (sucbv-count (aref data d) bit r)
                        (* bit (aref zeros d)))))
    (- r l)))

(defun wavelet-quantile (wmatrix k &optional (start 0) end)
  "Returns the K-th smallest number of WMATRIX in the range [START, END)"
  (declare ((integer 0 #.most-positive-fixnum) k start)
           ((or null (integer 0 #.most-positive-fixnum)) end))
  (let ((depth (wavelet-depth wmatrix))
        (length (wavelet-length wmatrix))
        (data (wavelet-data wmatrix))
        (zeros (wavelet-zeros wmatrix))
        (result 0))
    (declare (wavelet-integer result))
    (unless end
      (setq end length))
    (when (< (- end start) k)
      (error "The range [~A, ~A) contains less than ~A elements" start end k))
    (loop for d from (- depth 1) downto 0
          for lcount = (sucbv-rank (aref data d) start)
          for rcount = (sucbv-rank (aref data d) end)
          for zero-count of-type (integer 0 #.most-positive-fixnum)
             = (- (- end start) (- rcount lcount))
          do (if (<= zero-count k)
                 (setf start (+ lcount (aref zeros d))
                       end (+ rcount (aref zeros d))
                       k (- k zero-count)
                       result (logior result (the wavelet-integer (ash 1 d))))
                 (setf start (- start lcount)
                       end (- end rcount))))
    result))
