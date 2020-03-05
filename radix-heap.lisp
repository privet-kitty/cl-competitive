;;;
;;; Radix heap
;;;

(defconstant +radix-heap-bit-depth+ 32)
(deftype radix-heap-integer () '(unsigned-byte #.+radix-heap-bit-depth+))

(define-condition rheap-empty-error (error)
  ((rheap :initarg :rheap :reader rheap-empty-error-rheap))
  (:report
   (lambda (condition stream)
     (format stream "Attempted to pop empty heap ~W"
             (rheap-empty-error-rheap condition)))))

(define-condition rheap-not-monotone-error (error)
  ((rheap :initarg :rheap :reader rheap-not-monotone-error-rheap))
  (:report
   (lambda (condition stream)
     (format stream "Attempted to push an integer lower than the last popped one to heap ~W"
             (rheap-not-monotone-error-rheap condition)))))

(defstruct (radix-heap (:constructor make-radix-heap
                           (&aux (buckets (make-array (+ 1 +radix-heap-bit-depth+)
                                                      :element-type 'list
                                                      :initial-element nil))))
                       (:conc-name rheap-)
                       (:copier nil))
  (buckets nil :type (simple-array list (*)))
  (lowest 0 :type radix-heap-integer)
  (count 0 :type (integer 0 #.most-positive-fixnum))
  (stack nil :type list))

(declaim (inline rheap-push))
(defun rheap-push (obj rheap)
  (declare (radix-heap-integer obj))
  (let ((lowest (rheap-lowest rheap))
        (buckets (rheap-buckets rheap)))
    (when (< obj lowest)
      (error 'rheap-not-monotone-error :rheap rheap))
    (incf (rheap-count rheap))
    (let ((cell (or (rheap-stack rheap) (list obj)))
          (dest-index (integer-length (logxor obj lowest))))
      (pop (rheap-stack rheap))
      (rplaca cell obj)
      (rplacd cell (aref buckets dest-index))
      (setf (aref buckets dest-index) cell))
    rheap))

(declaim (inline rheap-pop))
(defun rheap-pop (rheap)
  (when (zerop (rheap-count rheap))
    (error 'rheap-empty-error :rheap rheap))
  (let ((buckets (rheap-buckets rheap)))
    (unless (aref buckets 0)
      (let* ((pos (position nil buckets :test-not #'eq))
             (bucket (aref buckets pos))
             (new-lowest #.(- (ash 1 +radix-heap-bit-depth+) 1)))
        (declare (radix-heap-integer new-lowest))
        (dolist (obj bucket)
          (setq new-lowest (min new-lowest (the radix-heap-integer obj))))
        (loop while bucket
              for cell = bucket
              for obj of-type radix-heap-integer = (car cell)
              for dest-index = (integer-length (logxor obj new-lowest))
              do (pop bucket)
                 (setf (cdr cell) (aref buckets dest-index)
                       (aref buckets dest-index) cell))
        (setf (rheap-lowest rheap) new-lowest)
        (setf (aref buckets pos) nil)))
    (decf (rheap-count rheap))
    (let ((cell (aref buckets 0)))
      (pop (aref buckets 0))
      (rplacd cell (rheap-stack rheap))
      (setf (rheap-stack rheap) cell)
      (car cell))))

(declaim (inline rheap-empty-p))
(defun rheap-empty-p (rheap)
  (zerop (rheap-count rheap)))

(defun bench (num)
  (declare (optimize (speed 3))
           (fixnum num))
  (let ((rheap (make-radix-heap))
        (state (sb-ext:seed-random-state 0)))
    (gc :full t)
    (time
     (dotimes (_ num)
       (if (or (zerop (rheap-count rheap)) (= (random 2 state) 1))
           (let* ((lowest (rheap-lowest rheap))
                  (rand (random most-positive-fixnum state))
                  (delta (logand (- (ash 1 (random 15 state)) 1) rand)))
             (rheap-push (+ lowest delta) rheap))
           (rheap-pop rheap))))
    (rheap-count rheap)))
