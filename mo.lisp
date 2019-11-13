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

(defun make-mo (bucket-width lefts rights)
  "LEFTS := vector of indices of left-end of queries (inclusive)
RIGHTS := vector of indices of right-end of queries (exclusive)

BUCKET-WIDTH would be better set to N/sqrt(Q) where N is the width of the
universe and Q is the number of queries."
  (declare (optimize (speed 3))
           ((simple-array mo-integer (*)) lefts rights)
           ((integer 0 #.most-positive-fixnum) bucket-width)
           (inline sort))
  (let* ((q (length lefts))
         (order (make-array q :element-type '(integer 0 #.most-positive-fixnum))))
    (assert (= q (length rights)))
    (dotimes (i q) (setf (aref order i) i))
    (setf order (sort order
                      (lambda (x y)
                        (if (= (floor (aref lefts x) bucket-width)
                               (floor (aref lefts y) bucket-width))
                            ;; Even-number [Odd-number] block is in ascending
                            ;; [descending] order w.r.t. the right boundary.
                            (if (evenp (floor (aref lefts x) bucket-width))
                                (< (aref rights x) (aref rights y))
                                (> (aref rights x) (aref rights y)))
                            (< (aref lefts x) (aref lefts y))))))
    (%make-mo lefts rights order bucket-width)))

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
  "Processes the next query. EXTEND and SHRINK take three arguments: the <index>
added/removed right now, and both ends of the next range: [<left>, <right>)"
  (declare (function extend shrink))
  (let* ((ord (mo-get-current mo))
         (left (aref (%mo-lefts mo) ord))
         (right (aref (%mo-rights mo) ord))
         (posl (%mo-posl mo))
         (posr (%mo-posr mo)))
    (declare ((integer 0 #.most-positive-fixnum) posl posr))
    (loop while (< left posl)
          do (decf posl)
             (funcall extend posl posl posr))
    (loop while (< posr right)
          do (funcall extend posr posl (+ posr 1))
             (incf posr))
    (loop while (< posl left)
          do (funcall shrink posl (+ posl 1) posr)
             (incf posl))
    (loop while (< right posr)
          do (decf posr)
             (funcall shrink posr posl posr))
    (setf (%mo-posl mo) posl
          (%mo-posr mo) posr)
    (incf (%mo-index mo))))

