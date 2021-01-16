;;;
;;; 2D range tree with fractional cascading
;;;
;;; build: O(nlog(n))
;;; query: O(log(n))
;;;
;;; Reference:
;;; Mark de Berg et al., Computational Geometry: Algorithms and Applications, 3rd Edition
;;;

(defpackage :cp/range-tree-fc
  (:use :cl)
  (:export #:make-range-tree #:rt-count #:rt-fold #:rt-map))
(in-package :cp/range-tree-fc)

;; TODO: introduce abelian group

(defstruct (ynode (:constructor make-ynode (xkeys ykeys lpointers rpointers values cumuls))
                  (:conc-name %ynode-)
                  (:copier nil))
  (xkeys nil :type (simple-array fixnum (*)))
  (ykeys nil :type (simple-array fixnum (*)))
  (lpointers nil :type (or null (simple-array (integer 0 #.most-positive-fixnum) (*))))
  (rpointers nil :type (or null (simple-array (integer 0 #.most-positive-fixnum) (*))))
  (values nil :type (simple-array fixnum (*)))
  (cumuls nil :type (or null (simple-array fixnum (*)))))

(defstruct (xnode (:constructor make-xnode (xkey ynode left right))
                  (:conc-name %xnode-)
                  (:copier nil))
  (xkey 0 :type fixnum)
  (ynode nil :type ynode)
  (left nil :type (or null xnode))
  (right nil :type (or null xnode)))

(defun %ynode-merge (ynode1 ynode2)
  "Merges two YNODEs non-destructively in O(n) time."
  (declare (optimize (speed 3)))
  (let* ((xkeys1 (%ynode-xkeys ynode1))
         (ykeys1 (%ynode-ykeys ynode1))
         (xkeys2 (%ynode-xkeys ynode2))
         (ykeys2 (%ynode-ykeys ynode2))
         (values1 (%ynode-values ynode1))
         (values2 (%ynode-values ynode2))
         (len1 (length xkeys1))
         (len2 (length xkeys2))
         (new-len (+ len1 len2))
         (new-xkeys (make-array new-len :element-type 'fixnum))
         (new-ykeys (make-array new-len :element-type 'fixnum))
         (new-values (make-array new-len :element-type 'fixnum))
         (new-cumuls (make-array (+ new-len 1) :element-type 'fixnum :initial-element 0))
         (lpointers (make-array (+ 1 new-len)
                                :element-type '(integer 0 #.most-positive-fixnum)))
         (rpointers (make-array (+ 1 new-len)
                                :element-type '(integer 0 #.most-positive-fixnum)))
         
         (new-pos 0)
         (pos1 0)
         (pos2 0))
    (declare ((integer 0 #.most-positive-fixnum) len1 len2 new-len new-pos pos1 pos2))
    ;; merge two vectors
    (loop
      (when (= pos1 len1)
        (loop
          for i from pos2 below len2
          do (setf (aref new-xkeys new-pos) (aref xkeys2 i)
                   (aref new-ykeys new-pos) (aref ykeys2 i)
                   (aref new-values new-pos) (aref values2 i)
                   (aref lpointers new-pos) pos1
                   (aref rpointers new-pos) i)
             (incf new-pos))
        (return))
      (when (= pos2 len2)
        (loop
          for i from pos1 below len1
          do (setf (aref new-xkeys new-pos) (aref xkeys1 i)
                   (aref new-ykeys new-pos) (aref ykeys1 i)
                   (aref new-values new-pos) (aref values1 i)
                   (aref lpointers new-pos) i
                   (aref rpointers new-pos) pos2)
             (incf new-pos))
        (return))
      (if (or (< (aref ykeys1 pos1) (aref ykeys2 pos2))
              (and (= (aref ykeys1 pos1) (aref ykeys2 pos2))
                   (< (aref xkeys1 pos1) (aref xkeys2 pos2))))
          (setf (aref new-xkeys new-pos) (aref xkeys1 pos1)
                (aref new-ykeys new-pos) (aref ykeys1 pos1)
                (aref new-values new-pos) (aref values1 pos1)
                (aref lpointers new-pos) pos1
                (aref rpointers new-pos) pos2
                pos1 (+ pos1 1))
          (setf (aref new-xkeys new-pos) (aref xkeys2 pos2)
                (aref new-ykeys new-pos) (aref ykeys2 pos2)
                (aref new-values new-pos) (aref values2 pos2)
                (aref lpointers new-pos) pos1
                (aref rpointers new-pos) pos2
                pos2 (+ pos2 1)))
      (incf new-pos))
    (dotimes (i new-len)
      (setf (aref new-cumuls (+ i 1))
            (+ (aref new-cumuls i) (aref new-values i))))
    (setf (aref lpointers new-len) len1
          (aref rpointers new-len) len2)
    (make-ynode new-xkeys new-ykeys lpointers rpointers new-values new-cumuls)))

(declaim (inline make-range-tree))
(defun make-range-tree (points &key (xkey #'car) (ykey #'cdr) value-key)
  "points := vector of points

Makes a range tree from the points. These points must be sorted
w.r.t. lexicographical order and must not contain duplicate points. (Duplicate
coordinates are allowed.) E.g. (-1, 3), (-1, 4), (-1, 7) (0, 1) (0, 3) (2,
-1) (2, 1))."
  (declare (vector points))
  (when (zerop (length points))
    (return-from make-range-tree nil))
  (let ((pointers-for-leaf
          (make-array 2
                      :element-type '(integer 0 #.most-positive-fixnum)
                      :initial-element 0)))
    (labels
        ((build (l r)
           (declare ((integer 0 #.most-positive-fixnum) l r))
           (if (= (- r l) 1)
               (let* ((point (aref points l))
                      (x (funcall xkey point))
                      (y (funcall ykey point))
                      (value (if value-key (funcall value-key point) 0))
                      (xkeys (make-array 1 :element-type 'fixnum :initial-element x))
                      (ykeys (make-array 1 :element-type 'fixnum :initial-element y))
                      (values (make-array 1 :element-type 'fixnum :initial-element value))
                      (cumuls (make-array 2 :element-type 'fixnum :initial-element 0)))
                 (setf (aref cumuls 1) value)
                 (make-xnode x (make-ynode xkeys ykeys
                                           pointers-for-leaf
                                           pointers-for-leaf
                                           values cumuls)
                             nil nil))
               (let* ((mid (ash (+ l r) -1))
                      (left (build l mid))
                      (right (build mid r)))
                 (make-xnode (funcall xkey (aref points mid))
                             (%ynode-merge (%xnode-ynode left)
                                           (%xnode-ynode right))
                             left right)))))
      (build 0 (length points)))))

(defconstant +neg-inf+ most-negative-fixnum)
(defconstant +pos-inf+ most-positive-fixnum)

(declaim (inline xleaf-p))
(defun xleaf-p (xnode)
  (and (null (%xnode-left xnode)) (null (%xnode-right xnode))))

(defun rt-count (range-tree x1 y1 x2 y2)
  "Returns the number of the nodes within the rectangle [x1, x2)*[y1, y2). A
part or all of these coordinates can be NIL; then they are regarded as the
negative or positive infinity."
  (declare (optimize (speed 3))
           ((or null fixnum) x1 y1 x2 y2))
  (setq x1 (or x1 +neg-inf+)
        x2 (or x2 +pos-inf+)
        y1 (or y1 +neg-inf+)
        y2 (or y2 +pos-inf+))
  (unless range-tree
    (return-from rt-count 0))
  (let* ((ynode (%xnode-ynode range-tree))
         (xkeys (%ynode-xkeys ynode))
         (ykeys (%ynode-ykeys ynode)))
    (labels ((bisect-left (y)
               (declare (fixnum y))
               (let ((left 0)
                     (ok (length xkeys)))
                 (declare ((integer 0 #.most-positive-fixnum) left ok))
                 (loop
                   (let ((mid (ash (+ left ok) -1)))
                     (if (= mid left)
                         (if (< (aref ykeys left) y)
                             (return ok)
                             (return left))
                         (if (< (aref ykeys mid) y)
                             (setq left mid)
                             (setq ok mid)))))))
             (recur (xnode x1 x2 start end)
               (declare ((or null xnode) xnode)
                        (fixnum x1 x2)
                        ;; KLUDGE: declaring ftype is not sufficient for the
                        ;; optimization on SBCL 1.1.14.
                        #+sbcl (values (integer 0 #.most-positive-fixnum)))
               (cond ((null xnode) 0)
                     ((and (= x1 +neg-inf+) (= x2 +pos-inf+))
                      (- end start))
                     (t
                      (let* ((xkey (%xnode-xkey xnode))
                             (ynode (%xnode-ynode xnode))
                             (lpointers (%ynode-lpointers ynode))
                             (rpointers (%ynode-rpointers ynode)))
                        (if (<= x1 xkey)
                            (if (< xkey x2)
                                ;; XKEY is in [X1, X2)
                                (if (xleaf-p xnode)
                                    (- end start)
                                    (+ (recur (%xnode-left xnode)
                                              x1 +pos-inf+
                                              (aref lpointers start)
                                              (aref lpointers end))
                                       (recur (%xnode-right xnode)
                                              +neg-inf+ x2
                                              (aref rpointers start)
                                              (aref rpointers end))))
                                ;; XKEY is in [X2, +inf)
                                (recur (%xnode-left xnode)
                                       x1 x2
                                       (aref lpointers start)
                                       (aref lpointers end)))
                            ;; XKEY is in (-inf, X1)
                            (recur (%xnode-right xnode)
                                   x1 x2
                                   (aref rpointers start)
                                   (aref rpointers end))))))))
      (let ((start (bisect-left y1))
            (end (bisect-left y2)))
        (recur range-tree x1 x2 start end)))))

(defun rt-fold (range-tree x1 y1 x2 y2)
  "Returns the sum of the values within the rectangle [x1, x2)*[y1, y2). A
part or all of these coordinates can be NIL; then they are regarded as the
negative or positive infinity."
  (declare (optimize (speed 3))
           ((or null fixnum) x1 y1 x2 y2))
  (setq x1 (or x1 +neg-inf+)
        x2 (or x2 +pos-inf+)
        y1 (or y1 +neg-inf+)
        y2 (or y2 +pos-inf+))
  (unless range-tree
    (return-from rt-fold 0))
  (let* ((ynode (%xnode-ynode range-tree))
         (xkeys (%ynode-xkeys ynode))
         (ykeys (%ynode-ykeys ynode)))
    (labels ((bisect-left (y)
               (declare (fixnum y))
               (let ((left 0)
                     (ok (length xkeys)))
                 (declare ((integer 0 #.most-positive-fixnum) left ok))
                 (loop
                   (let ((mid (ash (+ left ok) -1)))
                     (if (= mid left)
                         (if (< (aref ykeys left) y)
                             (return ok)
                             (return left))
                         (if (< (aref ykeys mid) y)
                             (setq left mid)
                             (setq ok mid)))))))
             (recur (xnode x1 x2 start end)
               (declare ((or null xnode) xnode)
                        (fixnum x1 x2)
                        ;; KLUDGE: declaring ftype is not sufficient for the
                        ;; optimization on SBCL 1.1.14.
                        #+sbcl (values fixnum))
               (if (null xnode)
                   0
                   (let* ((xkey (%xnode-xkey xnode))
                          (ynode (%xnode-ynode xnode))
                          (cumuls (%ynode-cumuls ynode))
                          (lpointers (%ynode-lpointers ynode))
                          (rpointers (%ynode-rpointers ynode)))
                     (if (and (= x1 +neg-inf+) (= x2 +pos-inf+))
                         (- (aref cumuls end) (aref cumuls start))
                         (if (<= x1 xkey)
                             (if (< xkey x2)
                                 ;; XKEY is in [X1, X2)
                                 (if (xleaf-p xnode)
                                     (- (aref cumuls end) (aref cumuls start))
                                     (+ (recur (%xnode-left xnode)
                                               x1 +pos-inf+
                                               (aref lpointers start)
                                               (aref lpointers end))
                                        (recur (%xnode-right xnode)
                                               +neg-inf+ x2
                                               (aref rpointers start)
                                               (aref rpointers end))))
                                 ;; XKEY is in [X2, +inf)
                                 (recur (%xnode-left xnode)
                                        x1 x2
                                        (aref lpointers start)
                                        (aref lpointers end)))
                             ;; XKEY is in (-inf, X1)
                             (recur (%xnode-right xnode)
                                    x1 x2
                                    (aref rpointers start)
                                    (aref rpointers end))))))))
      (let ((start (bisect-left y1))
            (end (bisect-left y2)))
        (recur range-tree x1 x2 start end)))))

;; not tested
(defun rt-map (function range-tree x1 y1 x2 y2)
  "Applies FUNCTION to all the points within the rectangle [x1, x2)*[y1, y2)."
  (declare (optimize (speed 3))
           ((or null fixnum) x1 y1 x2 y2)
           (function function))
  (setq x1 (or x1 +neg-inf+)
        x2 (or x2 +pos-inf+)
        y1 (or y1 +neg-inf+)
        y2 (or y2 +pos-inf+))
  (when range-tree
    (let* ((ynode (%xnode-ynode range-tree))
           (xkeys (%ynode-xkeys ynode))
           (ykeys (%ynode-ykeys ynode)))
      (labels ((bisect-left (y)
                 (declare (fixnum y))
                 (let ((left 0)
                       (ok (length xkeys)))
                   (declare ((integer 0 #.most-positive-fixnum) left ok))
                   (loop
                     (let ((mid (ash (+ left ok) -1)))
                       (if (= mid left)
                           (if (< (aref ykeys left) y)
                               (return ok)
                               (return left))
                           (if (< (aref ykeys mid) y)
                               (setq left mid)
                               (setq ok mid)))))))
               (recur (xnode x1 x2 start end)
                 (declare ((or null xnode) xnode)
                          (fixnum x1 x2))
                 (cond ((null xnode))
                       ((and (= x1 +neg-inf+) (= x2 +pos-inf+))
                        (loop with ynode = (%xnode-ynode xnode)
                              with xkeys = (%ynode-xkeys ynode)
                              with ykeys = (%ynode-ykeys ynode)
                              for i from start below end
                              for x = (aref xkeys i)
                              for y = (aref ykeys i)
                              do (funcall function x y)))
                       (t
                        (let* ((xkey (%xnode-xkey xnode))
                               (ynode (%xnode-ynode xnode))
                               (lpointers (%ynode-lpointers ynode))
                               (rpointers (%ynode-rpointers ynode)))
                          (if (<= x1 xkey)
                              (if (< xkey x2)
                                  ;; XKEY is in [X1, X2)
                                  (if (xleaf-p xnode)
                                      (loop with ynode = (%xnode-ynode xnode)
                                            with xkeys = (%ynode-xkeys ynode)
                                            with ykeys = (%ynode-ykeys ynode)
                                            for i from start below end
                                            for x = (aref xkeys i)
                                            for y = (aref ykeys i)
                                            do (funcall function x y))
                                      (progn
                                        (recur (%xnode-left xnode)
                                               x1 +pos-inf+
                                               (aref lpointers start)
                                               (aref lpointers end))
                                        (recur (%xnode-right xnode)
                                               +neg-inf+ x2
                                               (aref rpointers start)
                                               (aref rpointers end))))
                                  ;; XKEY is in [X2, +inf)
                                  (recur (%xnode-left xnode)
                                         x1 x2
                                         (aref lpointers start)
                                         (aref lpointers end)))
                              ;; XKEY is in (-inf, X1)
                              (recur (%xnode-right xnode)
                                     x1 x2
                                     (aref rpointers start)
                                     (aref rpointers end))))))))
        (let ((start (bisect-left y1))
              (end (bisect-left y2)))
          (recur range-tree x1 x2 start end))))))
