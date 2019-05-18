;;;
;;; Calculate inversion number by merge sort
;;;

;; Introduce INIT-VECTOR for better type-propagation on SBCL
#+sbcl
(eval-when (:compile-toplevel :load-toplevel :execute)
  (sb-c:defknown init-vector (vector)
      vector (sb-c:flushable)
    :overwrite-fndb-silently t)

  (sb-c:defoptimizer (init-vector sb-c:derive-type) ((template))
    (let* ((template-type (sb-c::lvar-type template))
           (spec `(,(if (sb-kernel:array-type-complexp template-type) 'array 'simple-array)
                   ,(sb-kernel:type-specifier (sb-kernel:array-type-element-type template-type))
                   (*))))
      (sb-c::careful-specifier-type spec))))

(defun init-vector (template)
  "Returns a newly initialized vector of the same type as TEMPLATE."
  (declare (optimize (speed 3)))
  (make-array (length template) :element-type (array-element-type template)))

(declaim (inline %merge-count))
(defun %merge-count (l mid r source-vec dest-vec predicate)
  (declare ((mod #.array-total-size-limit) l mid r)
           (function predicate))
  (loop with count of-type (integer 0 #.most-positive-fixnum) = 0
        with i = l
        with j = mid
        for idx from l
        when (= i mid)
        do (loop for j from j below r
                 for idx from idx
                 do (setf (aref dest-vec idx)
                          (aref source-vec j))
                 finally (return-from %merge-count count))
        when (= j r)
        do (loop for i from i below mid
                 for idx from idx
                 do (setf (aref dest-vec idx)
                          (aref source-vec i))
                 finally (return-from %merge-count count))
        do (if (funcall predicate
                        (aref source-vec j)
                        (aref source-vec i))
               (setf (aref dest-vec idx) (aref source-vec j)
                     j (1+ j)
                     count (+ count (- mid i)))
               (setf (aref dest-vec idx) (aref source-vec i)
                     i (1+ i)))))

(defmacro with-fixnum+ (form)
  (let ((fixnum+ '(integer 0 #.most-positive-fixnum)))
    `(the ,fixnum+
          ,(reduce (lambda (f1 f2)`(,(car form)
                                   (the ,fixnum+ ,f1)
                                   (the ,fixnum+ ,f2)))
	           (cdr form)))))

;; (declaim (inline %calc-by-bubble-sort!))
;; (defun %calc-by-bubble-sort! (vec predicate l r)
;;   (declare (function predicate)
;;            ((mod #.array-total-size-limit) l r))
;;   (loop with inv-count of-type (integer 0 #.most-positive-fixnum) = 0
;;         for end from r above l
;;         do (loop for i from l below (- end 1)
;;                  do (when (funcall predicate (aref vec (+ i 1)) (aref vec i))
;;                       (rotatef (aref vec i) (aref vec (+ i 1)))
;;                       (incf inv-count)))
;;         finally (return inv-count)))

(declaim (inline %calc-by-insertion-sort!))
(defun %calc-by-insertion-sort! (vec predicate l r)
  (declare (function predicate)
           ((mod #.array-total-size-limit) l r))
  (loop with inv-count of-type (integer 0 #.most-positive-fixnum) = 0
        for end from (+ l 1) below r
        do (loop for i from end above l
                 while (funcall predicate (aref vec i) (aref vec (- i 1)))
                 do (rotatef (aref vec (- i 1)) (aref vec i))
                    (incf inv-count))
        finally (return inv-count)))

(declaim (inline calc-inversion-number!))
(defun calc-inversion-number! (vector predicate &key (start 0) end)
  "Calculates the inversion number of VECTOR w.r.t. the strict order
PREDICATE. This function sorts VECTOR as a side effect."
  (declare (vector vector)
           (function predicate))
  (let ((end (or end (length vector))))
    (declare ((mod #.array-total-size-limit) start end))
    (assert (<= start end))
    (let ((buffer (init-vector vector)))
      (labels ((recurse (l r merge-to-vec1-p)
                 (declare (optimize (safety 0))
                          ((mod #.array-total-size-limit) l r))
                 (cond ((= l r) 0)
                       ((= (+ l 1) r)
                        (unless merge-to-vec1-p
                          (setf (aref buffer l) (aref vector l)))
                        0)
                       ;; ((and (<= (- r l) 24) merge-to-vec1-p)
                       ;;  (%calc-by-insertion-sort! vec1 predicate l r))
                       (t
                        (let ((mid (floor (+ l r) 2)))
                          (with-fixnum+
                              (+ (recurse l mid (not merge-to-vec1-p))
                                 (recurse mid r (not merge-to-vec1-p))
                                 (if merge-to-vec1-p
                                     (%merge-count l mid r buffer vector predicate)
                                     (%merge-count l mid r vector buffer predicate)))))))))
        (recurse start end t)))))

;; test
(defun calc-inversion-number-by-bubble-sort! (vec predicate)
  "PREDICATE must be strict order."
  (loop for end from (length vec) above 0
        sum (loop with inv-count = 0
                  for i from 0 below (- end 1)
                  do (when (funcall predicate (aref vec (+ i 1)) (aref vec i))
                       (rotatef (aref vec i) (aref vec (+ i 1)))
                       (incf inv-count))
                  finally (return inv-count))))

(defun test-inversion ()
  (let ((vec (make-array 200 :element-type 'fixnum)))
    (declare ((simple-array fixnum (200)) vec))
    (dotimes (i (length vec)) (setf (aref vec i) (random 20)))
    (assert (= (calc-inversion-number! (copy-seq vec) #'<)
               (calc-inversion-number-by-bubble-sort! (copy-seq vec) #'<)))))

(defun bench ()
  (let* ((seed (seed-random-state 0))
         (vector (make-array 1000000 :element-type 'fixnum)))
    (declare (optimize (speed 3))
             ((simple-array fixnum (1000000)) vector))
    (gc :full t)
    (time (loop repeat 20
                do (dotimes (i 1000000)
                     (setf (aref vector i) (random #.(expt 2 32) seed)))
                sum (calc-inversion-number! vector #'>) of-type fixnum))))
