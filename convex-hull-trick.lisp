;;;
;;; Convex Hull Trick for monotone slopes
;;;

(deftype cht-element-type () 'fixnum)

(define-condition cht-empty-error (simple-error)
  ((cht :initarg :cht :accessor cht-empty-error-cht))
  (:report (lambda (condition stream)
             (format stream
                     "Attempted to get a value on an empty CHT ~W"
                     (cht-empty-error-cht condition)))))

(define-condition cht-full-error (simple-error)
  ((cht :initarg :cht :accessor cht-full-error-cht))
  (:report (lambda (condition stream)
             (format stream
                     "Attempted to push a value on a full CHT ~W"
                     (cht-full-error-cht condition)))))

(defstruct (convex-hull-trick
            (:constructor make-cht
                (max-length
                 &optional (minimum t)
                 &aux
                 (slopes (make-array max-length :element-type 'cht-element-type))
                 (intercepts (make-array max-length :element-type 'cht-element-type))))
            (:conc-name %cht-)
            (:copier nil))
  (slopes nil :type (simple-array cht-element-type (*)))
  (intercepts nil :type (simple-array cht-element-type (*)))
  (minimum t :type boolean)
  (start 0 :type (integer 0 #.most-positive-fixnum))
  (length 0 :type (integer 0 #.most-positive-fixnum))
  (max-length 0 :type (integer 0 #.most-positive-fixnum)))

;; four basic operations on deque
(declaim (inline %cht-pop-back))
(defun %cht-pop-back (cht)
  (decf (%cht-length cht)))

(declaim (inline %cht-pop-front))
(defun %cht-pop-front (cht)
  (decf (%cht-length cht))
  (incf (%cht-start cht))
  (when (= (%cht-start cht) (%cht-max-length cht))
    (setf (%cht-start cht) 0)))

(declaim (inline %cht-push-back))
(defun %cht-push-back (cht slope intercept)
  (let ((pos (+ (%cht-start cht) (%cht-length cht))))
    (declare ((integer 0 #.most-positive-fixnum) pos))
    (when (>= pos (%cht-max-length cht))
      (decf pos (%cht-max-length cht)))
    (setf (aref (%cht-slopes cht) pos) slope
          (aref (%cht-intercepts cht) pos) intercept)
    (incf (%cht-length cht))))

(declaim (inline %cht-push-front))
(defun %cht-push-front (cht slope intercept)
  (let ((new-start (- (%cht-start cht) 1)))
    (when (= -1 new-start)
      (incf new-start (%cht-max-length cht)))
    (setf (aref (%cht-slopes cht) new-start) slope
          (aref (%cht-intercepts cht) new-start) intercept)
    (setf (%cht-start cht) new-start)
    (incf (%cht-length cht))))

(declaim (inline %removable-p))
(defun %removable-p (slope1 intercept1 slope2 intercept2 slope3 intercept3)
  "Returns true iff the **second** line is removable."
  (>= (* (- intercept3 intercept2)
         (- slope2 slope1))
      (* (- intercept2 intercept1)
         (- slope3 slope2))))

;; NOTE: The slopes of lines newly added to CHT must be largest or smallest
;; ever.
(defun cht-push (cht slope intercept)
  "Adds a new line to CHT."
  (declare (optimize (speed 3))
           (cht-element-type slope intercept))
  (when (= (%cht-length cht) (%cht-max-length cht))
    (error 'cht-full-error :cht cht))
  (unless (%cht-minimum cht)
    (setq slope (- slope)
          intercept (- intercept)))
  (let ((slopes (%cht-slopes cht))
        (intercepts (%cht-intercepts cht)))
    (labels ((ref (i)
               (let ((pos (+ (%cht-start cht) i)))
                 (declare ((integer 0 #.most-positive-fixnum) pos))
                 (when (>= pos (%cht-max-length cht))
                   (decf pos (%cht-max-length cht)))
                 (values (aref slopes pos) (aref intercepts pos)))))
      (cond ((zerop (%cht-length cht))
             (%cht-push-front cht slope intercept))
            ((>= slope (aref slopes (%cht-start cht)))
             ;; push the line to the front if SLOPE is larger than that of the head.
             (loop for start = (%cht-start cht)
                   while (and (>= (%cht-length cht) 2)
                              (let ((slope+1 (aref slopes start))
                                    (intercept+1 (aref intercepts start)))
                                (multiple-value-bind (slope+2 intercept+2) (ref 1)
                                  (declare (cht-element-type slope+1 intercept+1 slope+2 intercept+2))
                                  (%removable-p slope intercept
                                                slope+1 intercept+1
                                                slope+2 intercept+2))))
                   do (%cht-pop-front cht)
                   finally (%cht-push-front cht slope intercept)))
            (t
             ;; push the line to the end if SLOPE is smaller than that of the tail.
             ;; TODO: assert it.
             (loop for offset = (%cht-length cht)
                   while (and (>= offset 2)
                              (multiple-value-bind (slope-2 intercept-2) (ref (- offset 2))
                                (multiple-value-bind (slope-1 intercept-1) (ref (- offset 1))
                                  (declare (cht-element-type slope-2 intercept-2 slope-1 intercept-1))
                                  (%removable-p slope-2 intercept-2
                                                slope-1 intercept-1
                                                slope intercept))))
                   do (%cht-pop-back cht)
                   finally (%cht-push-back cht slope intercept))))
      cht)))

(declaim (inline cht-get))
(defun cht-get (cht x)
  "Returns the minimum (maximum) value at X. The time complexity is O(log(n))."
  (when (zerop (%cht-length cht))
    (error 'cht-empty-error :cht cht))
  (let ((ng -1)
        (ok (- (%cht-length cht) 1))
        (slopes (%cht-slopes cht))
        (intercepts (%cht-intercepts cht)))
    (declare ((integer -1 (#.array-total-size-limit)) ng ok))
    (labels ((calc (i)
               (let ((pos (+ (%cht-start cht) i)))
                 (declare ((integer 0 #.most-positive-fixnum) pos))
                 (when (>= pos (%cht-max-length cht))
                   (decf pos (%cht-max-length cht)))
                 (+ (* x (aref slopes pos))
                    (aref intercepts pos)))))
      (loop
        (when (<= (- ok ng) 1)
          (return
            (if (%cht-minimum cht)
                (calc ok)
                (- (calc ok)))))
        (let ((mid (ash (+ ng ok) -1)))
          (if (< (calc mid) (calc (+ mid 1)))
              (setq ok mid)
              (setq ng mid)))))))

(declaim (inline cht-increasing-get))
(defun cht-increasing-get (cht x)
  "Returns the minimum (maximum) value at X. The time complexity is O(1), though
X must be larger than the one given at the previous call of this function."
  (when (zerop (%cht-length cht))
    (error 'cht-empty-error :cht cht))
  (let ((slopes (%cht-slopes cht))
        (intercepts (%cht-intercepts cht)))
    (labels ((calc (slope intercept)
               (declare (cht-element-type slope intercept))
               (+ (* x slope) intercept)))
      (loop while (and (>= (%cht-length cht) 2)
                       (let* ((pos (%cht-start cht))
                              (slope0 (aref slopes pos))
                              (intercept0 (aref intercepts pos)))
                         (incf pos)
                         (when (= pos (%cht-max-length cht))
                           (setq pos 0))
                         (let ((slope1 (aref slopes pos))
                               (intercept1 (aref intercepts pos)))
                           (>= (calc slope0 intercept0)
                               (calc slope1 intercept1)))))
            do (%cht-pop-front cht))
      (let ((start (%cht-start cht)))
        (if (%cht-minimum cht)
            (calc (aref slopes start) (aref intercepts start))
            (- (calc (aref slopes start) (aref intercepts start))))))))

(declaim (inline cht-decreasing-get))
(defun cht-decreasing-get (cht x)
  "Returns the minimum (maximum) value at X. The time complexity is O(1), though
X must be smaller than the one given at the previous call of this function."
  (when (zerop (%cht-length cht))
    (error 'cht-empty-error :cht cht))
  (let ((slopes (%cht-slopes cht))
        (intercepts (%cht-intercepts cht)))
    (labels ((calc (slope intercept)
               (declare (cht-element-type slope intercept))
               (+ (* x slope) intercept))
             (get-last-idx ()
               (let ((idx (+ (%cht-start cht) (%cht-length cht) -1)))
                 (if (>= idx (%cht-max-length cht))
                     (- idx (%cht-max-length cht))
                     idx))))
      (loop while (and (>= (%cht-length cht) 2)
                       (let* ((pos (get-last-idx))
                              (slope-1 (aref slopes pos))
                              (intercept-1 (aref intercepts pos)))
                         (decf pos)
                         (when (< pos 0)
                           (incf pos (%cht-max-length cht)))
                         (let ((slope-2 (aref slopes pos))
                               (intercept-2 (aref intercepts pos)))
                           (>= (calc slope-1 intercept-1)
                               (calc slope-2 intercept-2)))))
            do (%cht-pop-back cht))
      (let ((end-1 (get-last-idx)))
        (if (%cht-minimum cht)
            (calc (aref slopes end-1) (aref intercepts end-1))
            (- (calc (aref slopes end-1) (aref intercepts end-1))))))))
