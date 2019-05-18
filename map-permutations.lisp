(declaim (inline map-permutations!))
(defun map-permutations! (function vector &optional (start 0) end)
  "Destructively permutes VECTOR[START] ... VECTOR[END-1] and applies FUNCTION
to VECTOR each time."
  (declare (function function)
           (vector vector))
  (labels ((recurse (start end)
             (declare ((mod #.array-total-size-limit) start end))
             (if (> start end)
                 (funcall function vector)
                 (progn
                   (recurse (1+ start) end)
                   (loop for i from (1+ start) below end
                         do (rotatef (aref vector start) (aref vector i))
                            (recurse (1+ start) end)
                            (rotatef (aref vector start) (aref vector i)))))))
    (recurse start (or end (length vector)))))

;; I introduce INIT-VECTOR for better type-propagation on SBCL.
#+sbcl
(eval-when (:compile-toplevel :load-toplevel :execute)
  (sb-c:defknown init-vector (vector (mod #.array-total-size-limit))
      vector (sb-c:flushable)
    :overwrite-fndb-silently t)

  (sb-c:defoptimizer (init-vector sb-c:derive-type) ((template size))
    (let* ((template-type (sb-c::lvar-type template))
           (size-value (multiple-value-bind (constantp value) (sb-c::constant-lvar-p size)
                         (when constantp value)))
           (spec `(,(if (sb-kernel:array-type-complexp template-type) 'array 'simple-array)
                   ,(sb-kernel:type-specifier (sb-kernel:array-type-element-type template-type))
                   (,(or size-value '*)))))
      (sb-c::careful-specifier-type spec))))

(defun init-vector (template size)
  "Returns a newly initialized vector of the same type as TEMPLATE vector with
SIZE."
  (declare (optimize (speed 3) (safety 0)))
  (make-array size :element-type (array-element-type template)))

(declaim (inline map-combinations))
(defun map-combinations (function vector &optional k)
  "Applies FUNCTION to each combination of length K of VECTOR. Note that
the vector passed to FUNCTION will be recycled."
  (declare (function function)
           (vector vector))
  (let* ((n (length vector))
         (k (or k n))
         (result (init-vector vector k)))
    (declare ((mod #.array-total-size-limit) k))
    (labels ((recurse (pos prev)
               (if (= pos k)
                   (funcall function result)
                   (loop for i from (1+ prev) below n
                         do (setf (aref result pos) (aref vector i))
                            (recurse (1+ pos) i)))))
      (recurse 0 -1))))

(declaim (inline map-permutations))
(defun map-permutations (function vector &optional k)
  "Applies FUNCTION to each permutation of length K of VECTOR. Note that the
vector passed to FUNCTION will be recycled."
  (declare (function function)
           (vector vector))
  (map-combinations (lambda (c) (map-permutations! function c)) vector k))

(defmacro do-permutations! ((var vector &optional (start 0) end) &body body)
  `(map-permutations! (lambda (,var) ,@body) ,vector ,start ,end))

(defmacro do-combinations ((var vector &optional k) &body body)
  `(map-combinations (lambda (,var) ,@body) ,vector ,k))

(defmacro do-permutations ((var vector &optional k) &body body)
  `(map-permutations (lambda (,var) ,@body) ,vector ,k))

;; (defun test (size)
;;   (let ((vector (make-array size :element-type '(unsigned-byte 32))))
;;     (declare ((simple-array (unsigned-byte 32) (*)) vector))
;;     (dotimes (i size) (setf (aref vector i) i))
;;     (map-permutations #'print vector (ash size -1))))
