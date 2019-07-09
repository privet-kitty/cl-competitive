;;;
;;; Mo's algorithm
;;;

(deftype mo-integer () 'fixnum)

(defstruct (mo (:constructor %make-mo
                   (lefts rights order width))
               (:conc-name %mo-)
               (:copier nil)
               (:predicate nil))
  (lefts nil :type (simple-array mo-integer (*)))
  (rights nil :type (simple-array mo-integer (*)))
  (order nil :type (simple-array (integer 0 #.most-positive-fixnum) (*)))
  (width 0 :type (integer 0 #.most-positive-fixnum))
  (index 0 :type (integer 0 #.most-positive-fixnum))
  (posl 0 :type mo-integer)
  (posr 0 :type mo-integer))

(defun make-mo (total-width lefts rights)
  "TOTAL-WIDTH is the width of the interval on which the queries exist. (NOT the
number of queries.)"
  (declare (optimize (speed 3))
           ((simple-array mo-integer (*)) lefts rights)
           ((integer 0 #.most-positive-fixnum) total-width)
           (inline sort))
  (let* ((q (length lefts))
         (order (make-array q :element-type '(integer 0 #.most-positive-fixnum)))
         (width (floor total-width (isqrt q))))
    (declare ((integer 0 #.most-positive-fixnum) width))
    (assert (= q (length rights)))
    (dotimes (i q) (setf (aref order i) i))
    (setf order (sort order
                      (lambda (x y)
                        (if (= (floor (aref lefts x) width)
                               (floor (aref lefts y) width))
                            ;; Even-number [Odd-number] block is in ascending
                            ;; [descending] order w.r.t. the right boundary.
                            (if (evenp (floor (aref lefts x) width))
                                (< (aref rights x) (aref rights y))
                                (> (aref rights x) (aref rights y)))
                            (< (aref lefts x) (aref lefts y))))))
    (%make-mo lefts rights order width)))

(declaim (inline mo-get-current))
(defun mo-get-current (mo)
  "Returns the original index of the current query."
  (aref (%mo-order mo) (%mo-index mo)))

(declaim (inline mo-get-previous))
(defun mo-get-previous (mo)
  "Returns the previous index of the current query. Returns the initial index
when no queries are processed yet."
  (aref (%mo-order mo) (max 0 (- (%mo-index mo) 1))))

(declaim (inline mo-process))
(defun mo-process (mo extend shrink)
  "Processes the next query."
  (declare (function extend shrink))
  (let* ((ord (mo-get-current mo))
         (left (aref (%mo-lefts mo) ord))
         (right (aref (%mo-rights mo) ord))
         (posl (%mo-posl mo))
         (posr (%mo-posr mo)))
    (declare ((integer 0 #.most-positive-fixnum) posl posr))
    (loop while (< left posl)
          do (decf posl)
             (funcall extend posl))
    (loop while (< posr right)
          do (funcall extend posr)
             (incf posr))
    (loop while (< posl left)
          do (funcall shrink posl)
             (incf posl))
    (loop while (< right posr)
          do (decf posr)
             (funcall shrink posr))
    (setf (%mo-posl mo) posl
          (%mo-posr mo) posr)
    (incf (%mo-index mo))))
