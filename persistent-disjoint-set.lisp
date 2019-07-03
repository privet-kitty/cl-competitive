;;;
;;; Partially persistent disjoint set
;;;

(defstruct (persistent-disjoint-set
            (:constructor make-persistent-disjoint-set
                (size
                 &aux
                 ;; DATA holds a negative integer as the size of the connected
                 ;; component and a non-negative integer as the parent.
                 (data (make-array size :element-type 'fixnum :initial-element -1))
                 ;; TIMESTAMPS records the time when each vertex is no longer a
                 ;; root.
                 (timestamps (make-array size
                                         :element-type '(integer 0 #.most-positive-fixnum)
                                         :initial-element most-positive-fixnum))
                 ;; HISTORY records changelog of each connected component: (time
                 ;; . size)
                 (history
                  (let ((res (make-array size :element-type '(vector (cons fixnum fixnum)))))
                    (dotimes (i size res)
                      (setf (aref res i)
                            (make-array 1 :element-type '(cons fixnum fixnum)
                                          :fill-pointer 1
                                          :initial-element (cons -1 1))))))))
            (:conc-name pds-))
  "partially persistent disjoint set"
  (data nil :type (simple-array fixnum (*)))
  (now 0 :type (integer 0 #.most-positive-fixnum))
  (timestamps nil :type (simple-array (integer 0 #.most-positive-fixnum) (*)))
  (history nil :type (simple-array (vector (cons fixnum fixnum)) (*))))

;; FIXME: add error handling of PDS-ROOT and PDS-CONNECTED-P. (It is too slow to
;; naively add this error to these functions.)
(define-condition persistent-disjoint-set-unknown-future (simple-error)
  ((disjoint-set :initarg :disjoint-set :reader pds-unknown-future-disjoint-set)
   (specified-time :initarg :specified-time :reader pds-unknown-future-specified-time))
  (:report
   (lambda (condition stream)
     (format stream "Attempted to query future information. Current time is ~W and specified time is ~W."
             (pds-now (pds-unknown-future-disjoint-set condition))
             (pds-unknown-future-specified-time condition)))))

(declaim (ftype (function * (values (integer 0 #.most-positive-fixnum) &optional)) pds-root))
(defun pds-root (x time disjoint-set)
  "Returns the root of X at TIME."
  (declare (optimize (speed 3))
           ((integer 0 #.most-positive-fixnum) x time))
  (if (< time (aref (pds-timestamps disjoint-set) x))
      x
      (pds-root (aref (pds-data disjoint-set) x) time disjoint-set)))

(declaim (inline pds-unite!))
(defun pds-unite! (x1 x2 disjoint-set)
  "Destructively unites X1 and X2."
  (declare ((or null (integer 0 #.most-positive-fixnum))))
  (symbol-macrolet ((now (pds-now disjoint-set)))
    (let ((time (+ 1 now)))
      (setf now time)
      (let ((timestamps (pds-timestamps disjoint-set))
            (data (pds-data disjoint-set))
            (root1 (pds-root x1 time disjoint-set))
            (root2 (pds-root x2 time disjoint-set)))
        (unless (= root1 root2)
          (when (> (aref data root1) (aref data root2))
            (rotatef root1 root2))
          ;; (size root1) >= (size root2)
          (incf (aref data root1) (aref data root2))
          (setf (aref data root2) root1
                (aref timestamps root2) time)
          (vector-push-extend (cons time (- (aref data root1)))
                              (aref (pds-history disjoint-set) root1))
          t)))))

(declaim (inline pds-connected-p))
(defun pds-connected-p (x1 x2 time disjoint-set)
  "Checks if X1 and X2 have the same root at TIME."
  (= (pds-root x1 time disjoint-set) (pds-root x2 time disjoint-set)))

(defun pds-opening-time (x1 x2 disjoint-set)
  "Returns the earliest time when X1 and X2 were connected. Returns NIL if X1
and X2 are not connected yet."
  ;; 
  (declare (optimize (speed 3))
           ((integer 0 #.most-positive-fixnum) x1 x2)
           (persistent-disjoint-set disjoint-set))
  (labels ((bisect (ng ok)
             (declare (optimize (safety 0))
                      ((integer 0 #.most-positive-fixnum) ng ok))
             (if (<= (- ok ng) 1)
                 ok
                 (let ((mid (ash (+ ng ok) -1)))
                   (if (pds-connected-p x1 x2 mid disjoint-set)
                       (bisect ng mid)
                       (bisect mid ok))))))
    (when (pds-connected-p x1 x2 (pds-now disjoint-set) disjoint-set)
      (bisect 0 (pds-now disjoint-set)))))

(declaim (ftype (function * (values (integer 0 #.most-positive-fixnum) &optional)) pds-size))
(defun pds-size (x time disjoint-set)
  "Returns the size of X at TIME."
  (declare (optimize (speed 3))
           ((integer 0 #.most-positive-fixnum) x time))
  (when (< (pds-now disjoint-set) time)
    (error 'persistent-disjoint-set-unknown-future :specified-time time :disjoint-set disjoint-set))
  (let* ((root (pds-root x time disjoint-set))
         (root-history (aref (pds-history disjoint-set) root)))
    (declare (optimize (safety 0)))
    ;; detect the latest time equal to or earlier than TIME 
    (labels ((bisect-left-1 (ok ng)
               (declare ((integer 0 #.most-positive-fixnum) ok ng))
               (if (<= (- ng ok) 1)
                   ok
                   (let ((mid (ash (+ ok ng) -1)))
                     (if (<= (the fixnum (car (aref root-history mid))) time)
                         (bisect-left-1 mid ng)
                         (bisect-left-1 ok mid))))))
      (cdr (aref root-history (bisect-left-1 0 (length root-history)))))))
