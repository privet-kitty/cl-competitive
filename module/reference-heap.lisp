;;;
;;; This is a slow implementation of binary heap. Please use abstract-heap
;;; instead. I leave it just for my reference.
;;;

(defpackage :cp/reference-heap
  (:use :cl)
  (:export #:heap #:heap-p #:make-heap #:heap-empty-error
           #:heap-push #:heap-pop #:heap-peek #:heap-empty-p #:heap-count #:heap-reinitialize))
(in-package :cp/reference-heap)

(defstruct (heap (:constructor make-heap
                            (size
                             &key order (element-type t)
                             &aux (data (make-array (1+ size) :element-type element-type))))
                 (:copier nil))
  (data nil :type (simple-array * (*)))
  (order #'< :type function :read-only t)
  (position 1 :type (integer 1 #.array-total-size-limit)))

(define-condition heap-empty-error (error)
  ((heap :initarg :heap :reader heap-empty-error-heap))
  (:report
   (lambda (condition stream)
     (format stream "Attempted to pop empty heap ~W" (heap-empty-error-heap condition)))))

(declaim (inline heap-push))
(defun heap-push (obj heap)
   "Adds OBJ to HEAP."
   (symbol-macrolet ((position (heap-position heap)))
     (when (>= position (length (heap-data heap)))
       (setf (heap-data heap)
               (adjust-array (heap-data heap)
                             (min (* position 2) (- array-total-size-limit 1)))))
     (let ((data (heap-data heap))
           (order (heap-order heap)))
       (labels ((heapify (pos)
                  (unless (= pos 1)
                    (let ((parent-pos (ash pos -1)))
                      (when
                          (funcall order (aref data pos) (aref data parent-pos))
                        (rotatef (aref data pos) (aref data parent-pos))
                        (heapify parent-pos))))))
         (setf (aref data position) obj)
         (heapify position)
         (incf position)
         heap))))

(declaim (inline heap-pop))
(defun heap-pop (heap)
  "Removes and returns the element at the top of HEAP."
  (symbol-macrolet ((position (heap-position heap)))
    (let ((data (heap-data heap))
          (order (heap-order heap)))
      (labels ((heapify (pos)
                 (declare ((integer 1 #.most-positive-fixnum) pos))
                 (let* ((child-pos1 (+ pos pos))
                        (child-pos2 (1+ child-pos1)))
                   (when (<= child-pos1 position)
                     (if (<= child-pos2 position)
                         (if (funcall order (aref data child-pos1) (aref data child-pos2))
                             (unless (funcall order (aref data pos) (aref data child-pos1))
                               (rotatef (aref data pos) (aref data child-pos1))
                               (heapify child-pos1))
                             (unless (funcall order (aref data pos) (aref data child-pos2))
                               (rotatef (aref data pos) (aref data child-pos2))
                               (heapify child-pos2)))
                         (unless (funcall order (aref data pos) (aref data child-pos1))
                           (rotatef (aref data pos) (aref data child-pos1))))))))
        (when (= position 1)
          (error 'heap-empty-error :heap heap))
        (prog1 (aref data 1)
          (decf position)
          (setf (aref data 1) (aref data position))
          (heapify 1))))))

(declaim (inline heap-reinitialize))
(defun heap-reinitialize (heap)
  (setf (heap-position heap) 1)
  heap)

(defun heap-peek (heap)
  "Returns the topmost element of HEAP."
  (if (= 1 (heap-position heap))
      (error 'heap-empty-error :heap heap)
      (aref (heap-data heap) 1)))

(declaim (inline heap-count))
(defun heap-count (heap)
  (- (heap-position heap) 1))

(declaim (inline heap-empty-p))
(defun heap-empty-p (heap)
  (= (heap-position heap) 1))
