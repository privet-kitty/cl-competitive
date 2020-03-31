;;;
;;; This is a slow implementation of binary heap. Please use abstract-heap
;;; instead. I leave it just for my reference.
;;;

(defstruct (heap (:constructor make-heap
                            (size
                             &key test (element-type t)
                             &aux (data (make-array (1+ size) :element-type element-type)))))
  (data nil :type (simple-array * (*)))
  (test #'< :type function :read-only t)
  (position 1 :type (integer 1 #.array-total-size-limit)))

(define-condition heap-empty-error (error)
  ((heap :initarg :heap :reader heap-empty-error-heap))
  (:report
   (lambda (condition stream)
     (format stream "Attempted to pop empty heap ~W" (heap-empty-error-heap condition)))))

(declaim (inline heap-push))
(defun heap-push (obj heap)
   "Adds obj to the end of heap."
   (symbol-macrolet ((position (heap-position heap)))
     (when (>= position (length (heap-data heap)))
       (setf (heap-data heap)
               (adjust-array (heap-data heap)
                             (min (* position 2) (- array-total-size-limit 1)))))
     (let ((data (heap-data heap))
           (test (heap-test heap)))
       (labels ((update (pos)
                  (unless (= pos 1)
                    (let ((parent-pos (ash pos -1)))
                      (when
                          (funcall test (aref data pos) (aref data parent-pos))
                        (rotatef (aref data pos) (aref data parent-pos))
                        (update parent-pos))))))
         (setf (aref data position) obj)
         (update position)
         (incf position)
         heap))))

(declaim (inline heap-pop))
(defun heap-pop (heap)
  (symbol-macrolet ((position (heap-position heap)))
    (let ((data (heap-data heap))
          (test (heap-test heap)))
      (labels ((update (pos)
                 (declare ((integer 1 #.most-positive-fixnum) pos))
                 (let* ((child-pos1 (+ pos pos))
                        (child-pos2 (1+ child-pos1)))
                   (when (<= child-pos1 position)
                     (if (<= child-pos2 position)
                         (if (funcall test (aref data child-pos1) (aref data child-pos2))
                             (unless (funcall test (aref data pos) (aref data child-pos1))
                               (rotatef (aref data pos) (aref data child-pos1))
                               (update child-pos1))
                             (unless (funcall test (aref data pos) (aref data child-pos2))
                               (rotatef (aref data pos) (aref data child-pos2))
                               (update child-pos2)))
                         (unless (funcall test (aref data pos) (aref data child-pos1))
                           (rotatef (aref data pos) (aref data child-pos1))))))))
        (if (= position 1)
            (error 'heap-empty-error :heap heap)
            (prog1 (aref data 1)
              (decf position)
              (setf (aref data 1) (aref data position))
              (update 1)))))))

(declaim (inline heap-reinitialize))
(defun heap-reinitialize (heap)
  (setf (heap-position heap) 1)
  heap)

(defun heap-peek (heap)
  (if (= 1 (heap-position heap))
      (error 'heap-empty-error :heap heap)
      (aref (heap-data heap) 1)))

(declaim (inline heap-count))
(defun heap-count (heap)
  (- (heap-position heap) 1))

(declaim (inline heap-empty-p))
(defun heap-empty-p (heap)
  (= (heap-position heap) 1))

;; (defun bench (&optional (size 2000000))
;;   (declare (optimize (speed 3)))
;;   (let* ((heap (make-heap size :element-type 'fixnum))
;;          (seed (seed-random-state 0)))
;;     (time (dotimes (i size)
;;             (heap-push (random most-positive-fixnum seed) heap)))
;;     (time (dotimes (i size)
;;             (heap-pop heap)))))
