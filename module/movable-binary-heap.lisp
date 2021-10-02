(defpackage :cp/movable-binary-heap
  (:use :cl)
  (:export #:make-heap #:heap-empty-error #:heap-pop
           #:heap-ensure-key #:heap-clear #:heap-peek #:heap-empty-p #:heap-count)
  (:documentation "Provides binary heap implementation with decrease-key and
increase-key operations."))
(in-package :cp/movable-binary-heap)

;; TODO: abstraction
;; NOTE: not tested

(declaim (inline compare))
(defun compare (x y)
  (declare (fixnum x y))
  (< x y))

(define-condition heap-empty-error (error)
  ((heap :initarg :heap :reader heap-empty-error-heap))
  (:report
   (lambda (condition stream)
     (format stream "Attempted to get an element from empty heap ~W"
             (heap-empty-error-heap condition)))))

(defstruct (heap
            (:constructor make-heap
                (size
                 &aux
                 (keys (make-array (+ 1 size) :element-type 'fixnum))
                 (priorities (make-array (+ 1 size) :element-type 'fixnum))
                 (positions (make-array (+ 1 size)
                                        :element-type 'fixnum
                                        :initial-element -1)))))
  (keys nil :type (simple-array fixnum (*)))
  (priorities nil :type (simple-array fixnum (*)))
  (positions nil :type (simple-array fixnum (*)))
  (end 1 :type (integer 1 (#.array-dimension-limit))))

(defun %heapify-up (heap pos)
  (declare (optimize (speed 3))
           ((mod #.array-dimension-limit) pos))
  (let* ((keys (heap-keys heap))
         (priorities (heap-priorities heap))
         (positions (heap-positions heap))
         (key (aref keys pos)))
    (labels ((recur (pos)
               (unless (= pos 1)
                 (let ((parent-pos (ash pos -1)))
                   (when (compare (aref priorities pos) (aref priorities parent-pos))
                     (rotatef (aref positions key) (aref positions (aref keys parent-pos)))
                     (rotatef (aref keys pos) (aref keys parent-pos))
                     (rotatef (aref priorities pos) (aref priorities parent-pos))
                     (recur parent-pos))))))
      (recur pos))))

(defun %heapify-down (heap pos)
  (declare (optimize (speed 3))
           ((mod #.array-dimension-limit) pos))
  (let* ((keys (heap-keys heap))
         (priorities (heap-priorities heap))
         (positions (heap-positions heap))
         (end (heap-end heap))
         (key (aref keys pos)))
    (labels
        ((recur (pos)
           (let* ((cpos1 (+ pos pos))
                  (cpos2 (+ 1 cpos1)))
             (declare ((mod #.array-dimension-limit) cpos1 cpos2))
             (when (< cpos1 end)
               (if (< cpos2 end)
                   (if (compare (aref priorities cpos1) (aref priorities cpos2))
                       (when (compare (aref priorities cpos1) (aref priorities pos))
                         (rotatef (aref positions key) (aref positions (aref keys cpos1)))
                         (rotatef (aref keys pos) (aref keys cpos1))
                         (rotatef (aref priorities pos) (aref priorities cpos1))
                         (recur cpos1))
                       (when (compare (aref priorities cpos2) (aref priorities pos))
                         (rotatef (aref positions key) (aref positions (aref keys cpos2)))
                         (rotatef (aref keys pos) (aref keys cpos2))
                         (rotatef (aref priorities pos) (aref priorities cpos2))
                         (recur cpos2)))
                   (when (compare (aref priorities cpos1) (aref priorities pos))
                     (rotatef (aref positions key) (aref positions (aref keys cpos1)))
                     (rotatef (aref keys pos) (aref keys cpos1))
                     (rotatef (aref priorities pos) (aref priorities cpos1))))))))
      (recur pos))))

(declaim #+sbcl (sb-ext:maybe-inline heap-ensure-key))
(defun heap-ensure-key (heap key priority &optional if-exists)
  "Adds KEY to HEAP if it doesn't exist and sets its priority to
PRIORITY. Otherwise updates its priority by IF-EXISTS."
  (declare (optimize (speed 3))
           ((mod #.array-dimension-limit) key))
  (symbol-macrolet ((end (heap-end heap)))
    ;; TODO: implement auto-extension
    ;; (when (>= end (length (heap-keys heap)))
    ;;   (let* ((new-size (min (* end 2) #.(- array-total-size-limit 1)))
    ;;          (new-keys (make-array new-size :element-type 'fixnum))
    ;;          (new-priorities (make-array new-size :element-type 'fixnum)))
    ;;     (replace new-keys (heap-keys heap))
    ;;     (replace new-priorities (heap-priorities heap))
    ;;     (setf (heap-keys heap) new-keys
    ;;           (heap-priorities heap) new-priorities)))
    (let ((keys (heap-keys heap))
          (priorities (heap-priorities heap))
          (positions (heap-positions heap)))
      (let ((pos (aref positions key)))
        (if (= pos -1)
            (progn
              (setf (aref keys end) key
                    (aref priorities end) priority
                    (aref positions key) end)
              (%heapify-up heap end)
              (incf end))
            (let ((prev-priority (aref priorities pos)))
              (setf (aref priorities pos)
                    (if if-exists
                        (funcall if-exists prev-priority)
                        priority))
              (if (compare prev-priority (aref priorities pos))
                  (%heapify-down heap pos)
                  (%heapify-up heap pos)))))
      heap)))

(declaim #+sbcl (sb-ext:maybe-inline heap-pop))
(defun heap-pop (heap)
  "Deletes the topmost element of HEAP and returns its key and priority."
  (declare (optimize (speed 3)))
  (symbol-macrolet ((end (heap-end heap)))
    (let ((keys (heap-keys heap))
          (priorities (heap-priorities heap))
          (positions (heap-positions heap)))
      (when (= end 1)
        (error 'heap-empty-error :heap heap))
      (multiple-value-prog1 (values (aref keys 1) (aref priorities 1))
        (decf end)
        (setf (aref positions (aref keys 1)) -1)
        (unless (= end 1)
          (setf (aref keys 1) (aref keys end)
                (aref priorities 1) (aref priorities end)
                (aref positions (aref keys 1)) 1))
        (%heapify-down heap 1)))))

(declaim (inline heap-clear))
(defun heap-clear (heap)
  "Makes heap empty. Note that it takes linear time."
  (setf (heap-end heap) 1)
  (fill (heap-positions heap) -1)
  heap)

(declaim (inline heap-empty-p))
(defun heap-empty-p (heap)
  "Returns true iff HEAP is empty."
  (= 1 (heap-end heap)))

(declaim (inline heap-count))
(defun heap-count (heap)
  "Returns the current number of the elements in HEAP."
  (- (heap-end heap) 1))

(declaim (inline heap-peek))
(defun heap-peek (heap)
  "Returns the topmost element of HEAP, i.e. its key and priority."
  (if (= 1 (heap-end heap))
      (error 'heap-empty-error :heap heap)
      (values (aref (heap-keys heap) 1)
              (aref (heap-priorities heap) 1))))
