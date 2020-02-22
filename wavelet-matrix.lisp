;;;
;;; Compact bit vector
;;;

;; NOTE: I adopt compact bit vector with POPCNT instruction because it is
;; usually better than the typical three-layer succinct bit vector (at least in
;; competitive programming).

(eval-when (:compile-toplevel :load-toplevel :execute)
  (assert (= sb-vm:n-word-bits 64)))
 
(defstruct (compact-bit-vector (:constructor %make-cbv (storage blocks))
                                (:conc-name cbv-)
                                (:copier nil))
  (storage nil :type simple-bit-vector)
  (blocks nil :type (simple-array (integer 0 #.most-positive-fixnum) (*))))
 
(defun make-cbv! (vector)
  "The consequence is undefined when VECTOR is modified after a compact bit
vector is created."
  (declare (optimize (speed 3)))
  (check-type vector simple-bit-vector)
  (let* ((vector (if (zerop (mod (length vector) 64))
                     vector
                     (adjust-array vector
                                   (* sb-vm:n-word-bits
                                      (ceiling (length vector) sb-vm:n-word-bits))
                                   :initial-element 0)))
         (len (length vector))
         (block-count (floor len sb-vm:n-word-bits))
         (blocks (make-array (+ 1 block-count)
                             :element-type '(integer 0 #.most-positive-fixnum)
                             :initial-element 0))
         (sum 0))
    (declare (simple-bit-vector vector)
             ((integer 0 #.most-positive-fixnum) sum))
    (dotimes (i block-count)
      (setf (aref blocks i) sum)
      (incf sum (logcount (sb-kernel:%vector-raw-bits vector i))))
    (setf (aref blocks block-count) sum)
    (%make-cbv vector blocks)))
 
(declaim (inline cbv-ref))
(defun cbv-ref (cbv index)
  (sbit (cbv-storage cbv) index))
 
;; NOTE: No error handling.
(declaim (inline cbv-rank)
         (ftype (function * (values (integer 0 #.most-positive-fixnum) &optional)) cbv-rank))
(defun cbv-rank (cbv end)
  "Counts the number of 1's in the range [0, END)."
  (declare ((integer 0 #.most-positive-fixnum) end))
  (let* ((storage (cbv-storage cbv))
         (blocks (cbv-blocks cbv))
         (bpos (ash end -6))
         (brem (logand #b111111 end)))
    (+ (aref blocks bpos)
       (if (zerop brem) ; avoid out-of-bounds access
           0
           (logcount (ldb (byte brem 0)
                          (sb-kernel:%vector-raw-bits storage bpos)))))))

(declaim (inline cbv-count)
         (ftype (function * (values (integer 0 #.most-positive-fixnum) &optional)) cbv-count))
(defun cbv-count (cbv value end)
  "Counts the number of VALUEs in the range [0, END)"
  (declare (bit value)
           ((integer 0 #.most-positive-fixnum) end))
  (let ((count1 (cbv-rank cbv end)))
    (if (= value 1)
        count1
        (- end count1))))

(defun cbv-select (cbv ord)
  "Detects the position of (1-based) ORD-th 1 in CBV. (CBV-SELECT 0) always
returns 0."
  (declare (optimize (speed 3))
           ((integer 0 #.most-positive-fixnum) ord))
  (let* ((storage (cbv-storage cbv))
         (blocks (cbv-blocks cbv))
         (block-size (length blocks)))
    (unless (<= ord (aref blocks (- block-size 1)))
      ;; FIXME: introduce condition class
      (error "~&There aren't ~W 1's in ~W" ord cbv))
    (labels ((block-bisect (ok ng)
               (declare ((unsigned-byte 32) ok ng))
               (if (<= (- ng ok) 1)
                   ok
                   (let ((mid (ash (+ ok ng) -1)))
                     (if (<= ord (aref blocks mid))
                         (block-bisect ok mid)
                         (block-bisect mid ng))))))
      (let* ((block-idx (block-bisect 0 block-size))
             (ord (- ord (aref blocks block-idx)))
             (word (sb-kernel:%vector-raw-bits storage block-idx)))
        (labels ((pos-bisect (ok ng)
                   (declare ((integer 0 #.sb-vm:n-word-bits) ok ng))
                   (if (<= (- ng ok) 1)
                       ok
                       (let ((mid (ash (+ ok ng) -1)))
                         (if (<= ord (logcount (ldb (byte mid 0) word)))
                             (pos-bisect ok mid)
                             (pos-bisect mid ng))))))
          (let ((pos (pos-bisect 0 sb-vm:n-word-bits)))
            (+ (* sb-vm:n-word-bits block-idx) pos)))))))

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
  (data nil :type (simple-array compact-bit-vector (*)))
  (zeros nil :type (simple-array (integer 0 #.most-positive-fixnum) (*))))

#+sbcl
(eval-when (:compile-toplevel :load-toplevel :execute)
  (sb-c:defknown make-wavelet ((integer 1 #.most-positive-fixnum) vector)
      wavelet-matrix (sb-c:flushable)
    :overwrite-fndb-silently t))

;; TODO: add deftransform for better type derivation
(defun make-wavelet (bit-depth vector)
  (declare ((integer 1 #.most-positive-fixnum) bit-depth))
  (let* ((len (length vector))
         (fitted-len (* sb-vm:n-word-bits (ceiling len sb-vm:n-word-bits)))
         (data (locally (declare #+sbcl (muffle-conditions style-warning))
                 (make-array bit-depth :element-type 'compact-bit-vector)))
         (zeros (make-array bit-depth :element-type '(integer 0 #.most-positive-fixnum)))
         (tmp (copy-seq vector))
         (lefts (make-array len :element-type (array-element-type vector)))
         (rights (make-array len :element-type (array-element-type vector)))
         (bits (make-array fitted-len :element-type 'bit)))
    (declare ((integer 0 #.most-positive-fixnum) len fitted-len)
             (vector tmp))
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
               (setf (aref data d) (make-cbv! (copy-seq bits))
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
  "Returns the value at INDEX."
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
          for bit = (cbv-ref sbv index)
          do (setq res (logior bit (ash res 1))
                   index (+ (cbv-count sbv bit index)
                            (* bit (aref zeros d)))))
    res))

(defun wavelet-count (wmatrix value l r)
  "Returns the number of VALUE in [L, R)"
  (declare (optimize (speed 3))
           (wavelet-integer value)
           ((integer 0 #.most-positive-fixnum) l r))
  (let ((depth (wavelet-depth wmatrix))
        (data (wavelet-data wmatrix))
        (zeros (wavelet-zeros wmatrix)))
    (unless (<= l r (wavelet-length wmatrix))
      (error 'invalid-wavelet-index-error :index (cons l r) :wavelet wmatrix))
    (loop for d from (- depth 1) downto 0
          for bit = (logand 1 (ash value (- d)))
          do (setq l (+ (cbv-count (aref data d) bit l)
                        (* bit (aref zeros d)))
                   r (+ (cbv-count (aref data d) bit r)
                        (* bit (aref zeros d)))))
    (- r l)))

(defun wavelet-kth-smallest (wmatrix k &optional (start 0) end)
  "Returns the (0-based) K-th smallest number of WMATRIX in the range [START,
END). Returns 2^<bit depth>-1 if K is equal to END - START."
  (declare (optimize (speed 3))
           ((integer 0 #.most-positive-fixnum) k start)
           ((or null (integer 0 #.most-positive-fixnum)) end))
  (let ((depth (wavelet-depth wmatrix))
        (end (or end (wavelet-length wmatrix)))
        (data (wavelet-data wmatrix))
        (zeros (wavelet-zeros wmatrix))
        (result 0))
    (declare (wavelet-integer result)
             ((integer 0 #.most-positive-fixnum) end))
    (when (< (- end start) k)
      (error "The range [~D, ~D) contains less than ~D elements" start end k))
    (loop for d from (- depth 1) downto 0
          for lcount = (cbv-rank (aref data d) start)
          for rcount = (cbv-rank (aref data d) end)
          for zero-count of-type (integer 0 #.most-positive-fixnum)
             = (- (- end start) (- rcount lcount))
          do (if (<= zero-count k)
                 (setq start (+ lcount (aref zeros d))
                       end (+ rcount (aref zeros d))
                       k (- k zero-count)
                       result (logior result (the wavelet-integer (ash 1 d))))
                 (setq start (- start lcount)
                       end (- end rcount))))
    result))

;; TODO: maybe better to integrate kth-smallest and kth-largest
(defun wavelet-kth-largest (wmatrix k &optional (start 0) end)
  "Returns the (0-based) K-th largest number of WMATRIX in the range [START, END)"
  (declare (optimize (speed 3))
           ((integer 0 #.most-positive-fixnum) k start)
           ((or null (integer 0 #.most-positive-fixnum)) end))
  (let ((depth (wavelet-depth wmatrix))
        (end (or end (wavelet-length wmatrix)))
        (data (wavelet-data wmatrix))
        (zeros (wavelet-zeros wmatrix))
        (result 0))
    (declare (wavelet-integer result)
             ((integer 0 #.most-positive-fixnum) end))
    (when (< (- end start) k)
      (error "The range [~D, ~D) contains less than ~D elements" start end k))
    (loop for d from (- depth 1) downto 0
          for lcount = (cbv-rank (aref data d) start)
          for rcount = (cbv-rank (aref data d) end)
          for one-count of-type (integer 0 #.most-positive-fixnum) = (- rcount lcount)
          do (if (> one-count k)
                 (setq start (+ lcount (aref zeros d))
                       end (+ rcount (aref zeros d))
                       result (logior result (the wavelet-integer (ash 1 d))))
                 (setq k (- k one-count)
                       start (- start lcount)
                       end (- end rcount))))
    result))

;; not tested
(defun wavelet-map-frequency (function wmatrix lo hi &optional (start 0) end)
  "Maps all values within [LO, HI). FUNCTION must take two arguments: value and
its frequency."
  (declare (optimize (speed 3))
           ((integer 0 #.most-positive-fixnum) lo hi start)
           ((or null (integer 0 #.most-positive-fixnum)) end)
           (function function))
  (let ((data (wavelet-data wmatrix))
        (zeros (wavelet-zeros wmatrix))
        (end (or end (wavelet-length wmatrix))))
    (assert (<= lo hi))
    (unless (<= start end (wavelet-length wmatrix))
      (error 'invalid-wavelet-index-error :index (cons start end) :wavelet wmatrix))
    (labels
        ((dfs (depth start end value)
           (declare ((integer 0 #.most-positive-fixnum) start end value)
                    ((integer -1 #.most-positive-fixnum) depth))
           (when (and (< value hi) (< start end))
             (if (= -1 depth)
                 (when (<= lo value)
                   (funcall function value (- end start)))
                 (let* ((next-value (logior value
                                            (the wavelet-integer (ash 1 depth))))
                        (upper-bound (logior next-value
                                             (- (the wavelet-integer (ash 1 depth)) 1))))
                   (when (<= lo upper-bound)
                     (let ((lcount (cbv-rank (aref data depth) start))
                           (rcount (cbv-rank (aref data depth) end)))
                       (dfs (- depth 1)
                            (- start lcount)
                            (- end rcount)
                            value)
                       (dfs (- depth 1)
                            (+ (aref zeros depth) lcount)
                            (+ (aref zeros depth) rcount)
                            next-value))))))))
      (dfs (- (wavelet-depth wmatrix) 1) start end 0))))

(defun wavelet-range-count (wmatrix lo hi &optional (start 0) end)
  "Returns the number of the integers within [LO, HI)."
  (declare (optimize (speed 3))
           ((integer 0 #.most-positive-fixnum) lo hi start)
           ((or null (integer 0 #.most-positive-fixnum)) end))
  (let ((data (wavelet-data wmatrix))
        (zeros (wavelet-zeros wmatrix))
        (end (or end (wavelet-length wmatrix))))
    (assert (<= lo hi))
    (unless (<= start end (wavelet-length wmatrix))
      (error 'invalid-wavelet-index-error :index (cons start end) :wavelet wmatrix))
    (labels
        ((dfs (depth start end value)
           (declare ((integer 0 #.most-positive-fixnum) start end value)
                    ((integer -1 #.most-positive-fixnum) depth)
                    #+sbcl (values (integer 0 #.most-positive-fixnum)))
           (cond ((or (= start end)
                      (<= hi value))
                  0)
                 ((= depth -1)
                  (if (< value lo)
                      0
                      (- end start)))
                 (t
                  (let* ((next-value (logior value
                                             (the wavelet-integer (ash 1 depth))))
                         (upper-bound (logior next-value
                                              (- (the wavelet-integer (ash 1 depth)) 1))))
                    (cond ((< upper-bound lo)
                           0)
                          ((and (<= lo value) (< upper-bound hi))
                           (- end start))
                          (t
                           (let ((lcount (cbv-rank (aref data depth) start))
                                 (rcount (cbv-rank (aref data depth) end)))
                             (+ (dfs (- depth 1)
                                     (- start lcount)
                                     (- end rcount)
                                     value)
                                (dfs (- depth 1)
                                     (+ (aref zeros depth) lcount)
                                     (+ (aref zeros depth) rcount)
                                     next-value))))))))))
      (dfs (- (wavelet-depth wmatrix) 1) start end 0))))
