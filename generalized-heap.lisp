;;;
;;; Binary heap
;;;

(define-condition heap-empty-error (simple-error)
  ((heap :initarg :heap :reader heap-empty-error-heap))
  (:report
   (lambda (condition stream)
     (format stream "Attempted to pop empty heap ~W" (heap-empty-error-heap condition)))))

(define-condition heap-full-error (simple-error)
  ((heap :initarg :heap :reader heap-full-error-heap)
   (item :initarg :item :reader heap-full-error-item))
  (:report
   (lambda (condition stream)
     (format stream "Attempted to push item ~W to full heap ~W"
             (heap-full-error-item condition)
             (heap-full-error-heap condition)))))

(defmacro define-binary-heap (name &key (order '#'>) (element-type 'fixnum))
  (check-type name symbol)
  (let* ((string-name (string name))
         (fname-push (intern (format nil "~A-PUSH" string-name)))
         (fname-pop (intern (format nil "~A-POP" string-name)))
         (fname-reinitialize (intern (format nil "~A-REINITIALIZE" string-name)))
         (fname-empty-p (intern (format nil "~A-EMPTY-P" string-name)))
         (fname-peak (intern (format nil "~A-PEAK" string-name)))
         (fname-make (intern (format nil "MAKE-~A" string-name)))
         (acc-position (intern (format nil "~A-POSITION" string-name)))
         (acc-data (intern (format nil "~A-DATA" string-name))))
    `(progn
       (defstruct (,name
                   (:constructor ,fname-make
                       (size
                        &aux (data ,(if (eql element-type '*)
                                        `(make-array (1+ size))
                                        `(make-array (1+ size) :element-type ',element-type))))))
         (data #() :type (simple-array ,element-type (*)) :read-only t)
         (position 1 :type (integer 1 #.most-positive-fixnum)))

       (declaim (inline ,fname-push))
       (defun ,fname-push (obj heap)
         "Adds OBJ to the end of HEAP."
         (symbol-macrolet ((position (,acc-position heap)))
           (let ((data (,acc-data heap)))
             (declare ((simple-array ,element-type (*)) data))
             (labels ((update (pos)
                        (unless (= pos 1)
                          (let ((parent-pos (ash pos -1)))
                            (when (funcall ,order (aref data pos) (aref data parent-pos))
                              (rotatef (aref data pos) (aref data parent-pos))
                              (update parent-pos))))))
               (unless (< position (length data))
                 (error 'heap-full-error :heap heap :item obj))
               (setf (aref data position) obj)
               (update position)
               (incf position)
               heap))))

       (declaim (inline ,fname-pop))
       (defun ,fname-pop (heap)
         "Pops an element from the top of HEAP."
         (symbol-macrolet ((position (,acc-position heap)))
           (let ((data (,acc-data heap)))
             (declare ((simple-array ,element-type (*)) data))
             (labels ((update (pos)
                        (declare ((integer 1 #.most-positive-fixnum) pos))
                        (let* ((child-pos1 (+ pos pos))
                               (child-pos2 (1+ child-pos1)))
                          (when (<= child-pos1 position)
                            (if (<= child-pos2 position)
                                (if (funcall ,order (aref data child-pos1) (aref data child-pos2))
                                    (unless (funcall ,order (aref data pos) (aref data child-pos1))
                                      (rotatef (aref data pos) (aref data child-pos1))
                                      (update child-pos1))
                                    (unless (funcall ,order (aref data pos) (aref data child-pos2))
                                      (rotatef (aref data pos) (aref data child-pos2))
                                      (update child-pos2)))
                                (unless (funcall ,order (aref data pos) (aref data child-pos1))
                                  (rotatef (aref data pos) (aref data child-pos1))))))))
               (when (= position 1)
                 (error 'heap-empty-error :heap heap))
               (prog1 (aref data 1)
                 (decf position)
                 (setf (aref data 1) (aref data position))
                 (update 1))))))

       (declaim (inline ,fname-reinitialize))
       (defun ,fname-reinitialize (heap)
         (setf (,acc-position heap) 1)
         heap)

       (declaim (inline ,fname-empty-p))
       (defun ,fname-empty-p (heap)
         (= 1 (,acc-position heap)))

       (declaim (inline ,fname-peak))
       (defun ,fname-peak (heap)
         (if (= 1 (,acc-position heap))
             (error 'heap-empty-error :heap heap)
             (aref (,acc-data heap) 1))))))

(define-binary-heap heap
  :order #'>
  :element-type fixnum)

;; (defun bench (&optional (size 2000000))
;;   (declare (fixnum size) (optimize (speed 3)))
;;   (let* ((heap (make-heap size))
;;          (seed (seed-random-state 0)))
;;     (time (dotimes (i size)
;;             (heap-push (random most-positive-fixnum seed) heap)))
;;     (time (dotimes (i size)
;;             (heap-pop heap)))))
