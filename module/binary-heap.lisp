(defpackage :cp/binary-heap
  (:use :cl)
  (:export #:define-binary-heap #:heap-empty-error #:heap-empty-error-heap)
  (:documentation "Provides binary heap."))
(in-package :cp/binary-heap)

(define-condition heap-empty-error (error)
  ((heap :initarg :heap :reader heap-empty-error-heap))
  (:report
   (lambda (condition stream)
     (format stream "Attempted to get an element from empty heap ~W"
             (heap-empty-error-heap condition)))))

(defmacro define-binary-heap (name &key order (element-type 'fixnum))
  "Defines a binary heap specialized for the given order and the element
type. This macro defines a structure of the given NAME and relevant functions:
MAKE-<NAME>, <NAME>-PUSH, <NAME>-POP, <NAME>-CLEAR, <NAME>-EMPTY-P,
<NAME>-COUNT, <NAME>-PEEK, and <NAME>-MAP.

If ORDER is not given, heap for dynamic order is defined instead, and the
constructor takes an order function as an argument. Note that it will be
slightly slower than a static order, as it cannot be inlined."
  (check-type name symbol)
  (let* ((string-name (string name))
         (fname-push (intern (format nil "~A-PUSH" string-name)))
         (fname-pop (intern (format nil "~A-POP" string-name)))
         (fname-clear (intern (format nil "~A-CLEAR" string-name)))
         (fname-empty-p (intern (format nil "~A-EMPTY-P" string-name)))
         (fname-count (intern (format nil "~A-COUNT" string-name)))
         (fname-peek (intern (format nil "~A-PEEK" string-name)))
         (fname-make (intern (format nil "MAKE-~A" string-name)))
         (fname-map (intern (format nil "~A-MAP" string-name)))
         (acc-position (intern (format nil "~A-POSITION" string-name)))
         (acc-data (intern (format nil "~A-DATA" string-name)))
         (acc-order (intern (format nil "~A-ORDER" string-name)))
         (dynamic-order (null order))
         (order (or order 'order)))
    `(progn
       (defstruct (,name
                   (:constructor ,fname-make
                       (size
                        ,@(when dynamic-order '(order))
                        &aux
                        (data
                         (locally
                             (declare #+sbcl (sb-ext:muffle-conditions style-warning))
                           (make-array (1+ size) :element-type ',element-type))))))
         (data nil :type (simple-array ,element-type (*)))
         (position 1 :type (integer 1 #.array-dimension-limit))
         ,@(when dynamic-order
             `((order nil :type function))))

       (declaim #+sbcl (sb-ext:maybe-inline ,fname-push))
       (defun ,fname-push (obj heap)
         "Inserts OBJ to HEAP."
         (declare (optimize (speed 3))
                  (type ,name heap))
         (symbol-macrolet ((position (,acc-position heap)))
           (when (>= position (length (,acc-data heap)))
             (setf (,acc-data heap)
                   (adjust-array (,acc-data heap)
                                 (min (- array-dimension-limit 1)
                                      (* position 2)))))
           (let ((data (,acc-data heap))
                 ,@(when dynamic-order `((order (,acc-order heap)))))
             (declare ((simple-array ,element-type (*)) data))
             (labels ((heapify (pos)
                        (declare (optimize (speed 3) (safety 0))
                                 ((mod #.array-dimension-limit) pos))
                        (unless (= pos 1)
                          (let ((parent-pos (ash pos -1)))
                            (when (funcall ,order (aref data pos) (aref data parent-pos))
                              (rotatef (aref data pos) (aref data parent-pos))
                              (heapify parent-pos))))))
               (setf (aref data position) obj)
               (heapify position)
               (incf position)
               heap))))

       (declaim #+sbcl (sb-ext:maybe-inline ,fname-pop))
       (defun ,fname-pop (heap)
         "Removes and returns the element at the top of HEAP. Signals
HEAP-EMPTY-ERROR if HEAP is empty."
         (declare (optimize (speed 3))
                  (type ,name heap))
         (symbol-macrolet ((position (,acc-position heap)))
           (let ((data (,acc-data heap))
                 ,@(when dynamic-order `((order (,acc-order heap)))))
             (declare ((simple-array ,element-type (*)) data))
             (labels
                 ((heapify (pos)
                    (declare (optimize (speed 3) (safety 0))
                             ((mod #.array-dimension-limit) pos))
                    (let* ((child-pos1 (+ pos pos))
                           (child-pos2 (1+ child-pos1)))
                      (declare ((mod #.array-dimension-limit) child-pos1 child-pos2))
                      (when (<= child-pos1 position)
                        (if (<= child-pos2 position)
                            (if (funcall ,order (aref data child-pos1) (aref data child-pos2))
                                (unless (funcall ,order (aref data pos) (aref data child-pos1))
                                  (rotatef (aref data pos) (aref data child-pos1))
                                  (heapify child-pos1))
                                (unless (funcall ,order (aref data pos) (aref data child-pos2))
                                  (rotatef (aref data pos) (aref data child-pos2))
                                  (heapify child-pos2)))
                            (unless (funcall ,order (aref data pos) (aref data child-pos1))
                              (rotatef (aref data pos) (aref data child-pos1))))))))
               (when (= position 1)
                 (error 'heap-empty-error :heap heap))
               (prog1 (aref data 1)
                 (decf position)
                 (setf (aref data 1) (aref data position))
                 (heapify 1))))))

       (declaim (inline ,fname-clear))
       (defun ,fname-clear (heap)
         "Makes HEAP empty."
         (setf (,acc-position heap) 1)
         heap)

       (declaim (inline ,fname-empty-p))
       (defun ,fname-empty-p (heap)
         "Returns true iff HEAP is empty."
         (= 1 (,acc-position heap)))

       (declaim (inline ,fname-count))
       (defun ,fname-count (heap)
         "Returns the current number of the elements in HEAP."
         (- (,acc-position heap) 1))

       (declaim (inline ,fname-peek))
       (defun ,fname-peek (heap)
         "Returns the topmost element of HEAP. Signals HEAP-EMPTY-ERROR if HEAP
is empty."
         (if (= 1 (,acc-position heap))
             (error 'heap-empty-error :heap heap)
             (aref (,acc-data heap) 1)))

       (declaim (inline ,fname-map))
       (defun ,fname-map (function heap)
         "Applies FUNCTION to all the elements in HEAP. Note that the order in
which the elements in HEAP are passed to FUNCTION is not defined."
         (let ((data (,acc-data heap)))
           (loop for i from 1 below (,acc-position heap)
                 do (funcall function (aref data i))))))))

#+(or)
(define-binary-heap heap
  :order #'>
  :element-type fixnum)
