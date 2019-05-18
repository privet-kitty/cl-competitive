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
         (symbol-macrolet ((position (,acc-position heap)))
           (let ((data (,acc-data heap)))
             (declare ((simple-array ,element-type (*)) data))
             (labels ((update (pos)
                        (unless (= pos 1)
                          (let ((parent-pos (ash pos -1)))
                            (when (funcall ,order (aref data pos) (aref data parent-pos))
                              (rotatef (aref data pos) (aref data parent-pos))
                              (update parent-pos))))))
               (setf (aref data position) obj)
               (update position)
               (incf position)
               heap))))

       (declaim (inline ,fname-pop))
       (defun ,fname-pop (heap &optional (error t) null-value)
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
               (if (= position 1)
                   (if error
                       (error "No element in heap.")
                       null-value)
                   (prog1 (aref data 1)
                     (decf position)
                     (setf (aref data 1) (aref data position))
                     (update 1)))))))

       (declaim (inline ,fname-reinitialize))
       (defun ,fname-reinitialize (heap)
         (setf (,acc-position heap) 1)
         heap)

       (declaim (inline ,fname-empty-p))
       (defun ,fname-empty-p (heap)
         (= 1 (,acc-position heap)))

       (declaim (inline ,fname-peak))
       (defun ,fname-peak (heap &optional (error t) null-value)
         (if (= 1 (,acc-position heap))
             (if error
                 (error "No element in heap")
                 null-value)
             (aref (,acc-data heap) 1))))))

(define-binary-heap heap
  :order #'>
  :element-type fixnum)

;; For order
;; (eval-when (:compile-toplevel :load-toplevel :execute)
;;   (ql:quickload :fiveam)
;;   (use-package :fiveam))

;; (define-binary-heap my-heap
;;   :order #'<
;;   :element-type (unsigned-byte 32))

;; (test my-heap-test
;;   (let ((h (make-my-heap 20)))
;;     (finishes (dolist (o (list 7 18 22 15 27 9 11))
;;                 (my-heap-push o h)))
;;     (is (= 7 (my-heap-peak h)))
;;     (is (equal '(7 9 11 15 18 22 27)
;;                (loop repeat 7 collect (my-heap-pop h))))
;;     (signals error (my-heap-pop h))
;;     (is (eql 'eof (my-heap-pop h nil 'eof)))
;;     (is (eql 'eof (my-heap-peak h nil 'eof))))
;;   (is (typep (my-heap-data (make-my-heap 10))
;;              '(simple-array (unsigned-byte 32) (*)))))

;; (run! 'my-heap-test)

;; (defun bench (&optional (size 2000000))
;;   (declare (fixnum size) (optimize (speed 3)))
;;   (let* ((heap (make-heap size))
;;          (seed (seed-random-state 0)))
;;     (time (dotimes (i size)
;;             (heap-push (random most-positive-fixnum seed) heap)))
;;     (time (dotimes (i size)
;;             (heap-pop heap)))))
