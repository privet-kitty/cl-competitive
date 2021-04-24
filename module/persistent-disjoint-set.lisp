;;;
;;; Partially persistent disjoint set
;;;

(defpackage :cp/persistent-disjoint-set
  (:use :cl)
  (:export #:persistent-disjoint-set #:make-persistent-disjoint-set
           #:pds-now #:pds-timestamps #:pds-data #:pds-history
           #:persistent-disjoint-set-query-future
           #:pds-root #:pds-unite! #:pds-connected-p #:pds-opening-time #:pds-size))
(in-package :cp/persistent-disjoint-set)

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
                 ;; HISTORY records changelog of each connected component: time and size
                 (history
                  (let ((res (make-array size :element-type '(simple-array fixnum (*)))))
                    (dotimes (i size res)
                      (setf (aref res i)
                            (make-array 2 :element-type 'fixnum :initial-contents '(-1 1))))))
                 (ends (make-array size :element-type '(integer 0 #.most-positive-fixnum) :initial-element 2))))
            (:conc-name pds-)
            (:copier nil)
            (:predicate nil))
  "partially persistent disjoint set"
  (data nil :type (simple-array fixnum (*)))
  (now 0 :type (integer 0 #.most-positive-fixnum))
  (timestamps nil :type (simple-array (integer 0 #.most-positive-fixnum) (*)))
  (history nil :type (simple-array (simple-array fixnum (*)) (*)))
  (ends nil :type (simple-array (integer 0 #.most-positive-fixnum) (*))))

;; FIXME: add error handling of PDS-ROOT and PDS-CONNECTED-P. (It is too slow to
;; naively add this error to these functions.)
(define-condition persistent-disjoint-set-query-future (error)
  ((disjoint-set :initarg :disjoint-set :reader pds-query-future-disjoint-set)
   (specified-time :initarg :specified-time :reader pds-query-future-specified-time))
  (:report
   (lambda (condition stream)
     (format stream "Attempted to query future information. Current time is ~W and specified time is ~W."
             (pds-now (pds-query-future-disjoint-set condition))
             (pds-query-future-specified-time condition)))))

(declaim (inline pds-root)
         (ftype (function * (values (integer 0 #.most-positive-fixnum) &optional)) pds-root))
(defun pds-root (disjoint-set x time)
  "Returns the root of X at TIME."
  (declare ((integer 0 #.most-positive-fixnum) x time))
  (let ((data (pds-data disjoint-set))
        (timestamps (pds-timestamps disjoint-set)))
    (labels ((recur (x)
               (if (< time (aref timestamps x))
                   x
                   (recur (aref data x)))))
      (recur x))))

(declaim (inline pds-unite!))
(defun pds-unite! (disjoint-set x1 x2)
  "Destructively unites X1 and X2."
  (declare ((or null (integer 0 #.most-positive-fixnum))))
  (symbol-macrolet ((now (pds-now disjoint-set)))
    (let ((time (+ 1 now)))
      (setf now time)
      (let ((timestamps (pds-timestamps disjoint-set))
            (data (pds-data disjoint-set))
            (history (pds-history disjoint-set))
            (ends (pds-ends disjoint-set))
            (root1 (pds-root disjoint-set x1 time))
            (root2 (pds-root disjoint-set x2 time)))
        (unless (= root1 root2)
          ;; ensure (size root1) >= (size root2)
          (when (> (aref data root1) (aref data root2))
            (rotatef root1 root2))
          (incf (aref data root1) (aref data root2))
          (setf (aref data root2) root1
                (aref timestamps root2) time)
          (when (= (aref ends root1) (length (aref history root1)))
            (setf (aref history root1)
                  (adjust-array (aref history root1)
                                (the (mod #.array-dimension-limit)
                                     (* 2 (aref ends root1))))))
          (setf (aref (aref history root1) (aref ends root1)) time)
          (setf (aref (aref history root1) (+ 1 (aref ends root1)))
                (- (aref data root1)))
          (incf (aref ends root1) 2)
          t)))))

(declaim (inline pds-connected-p))
(defun pds-connected-p (disjoint-set x1 x2 time)
  "Returns true iff X1 and X2 have the same root at TIME."
  (= (pds-root disjoint-set x1 time) (pds-root disjoint-set x2 time)))

(defun pds-opening-time (disjoint-set x1 x2)
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
                   (if (pds-connected-p disjoint-set x1 x2 mid)
                       (bisect ng mid)
                       (bisect mid ok))))))
    (when (pds-connected-p disjoint-set x1 x2 (pds-now disjoint-set))
      (bisect 0 (pds-now disjoint-set)))))

(declaim (ftype (function * (values (integer 0 #.most-positive-fixnum) &optional)) pds-size))
(defun pds-size (disjoint-set x time)
  "Returns the size of X at TIME."
  (declare (optimize (speed 3))
           ((integer 0 #.most-positive-fixnum) x time))
  (when (< (pds-now disjoint-set) time)
    (error 'persistent-disjoint-set-query-future :specified-time time :disjoint-set disjoint-set))
  (let* ((root (pds-root disjoint-set x time))
         (root-history (aref (pds-history disjoint-set) root)))
    (declare (optimize (safety 0)))
    ;; detect the latest time equal to or earlier than TIME 
    (labels ((bisect-left-1 (ok ng)
               (declare ((integer 0 #.most-positive-fixnum) ok ng))
               (if (<= (- ng ok) 2)
                   ok
                   (let ((mid (logand -2 (ash (+ ok ng) -1))))
                     (if (<= (aref root-history mid) time)
                         (bisect-left-1 mid ng)
                         (bisect-left-1 ok mid))))))
      (aref root-history
            (+ 1 (bisect-left-1 0 (aref (pds-ends disjoint-set) root)))))))
