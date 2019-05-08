;;;
;;; Partially persistent disjoint set
;;;

;; TODO: enable to get the size of a connected component
(defstruct (persistent-disjoint-set
            (:constructor make-persistent-disjoint-set
                (size &aux
                      ;; DATA holds a negative integer as the size of the
                      ;; connected component and a non-negative integer as
                      ;; the parent.
                      (data (make-array size :element-type 'fixnum :initial-element -1))
                      ;; TIMESTAMPS records the time when each vertex is no
                      ;; longer a root.
                      (timestamps (make-array size :element-type '(integer 0 #.most-positive-fixnum)
                                                   :initial-element most-positive-fixnum))))
            (:conc-name pds-))
  "partially persistent disjoint set"
  (data nil :type (simple-array fixnum (*)))
  (now 0 :type (integer 0 #.most-positive-fixnum))
  (timestamps nil :type (simple-array (integer 0 #.most-positive-fixnum) (*))))

(declaim (ftype (function * (values (integer 0 #.most-positive-fixnum) &optional)) pds-root))
(defun pds-root (x time disjoint-set)
  "Returns the root of X at TIME."
  (declare (optimize (speed 3))
           ((integer 0 #.most-positive-fixnum) x time))
  (if (< time (aref (pds-timestamps disjoint-set) x))
      x
      (pds-root (aref (pds-data disjoint-set) x) time disjoint-set)))

(declaim (inline pds-unite!))
(defun pds-unite! (x1 x2 disjoint-set &optional time)
  "Destructively unites X1 and X2."
  (declare ((or null (integer 0 #.most-positive-fixnum))))
  (symbol-macrolet ((now (pds-now disjoint-set)))
    (let ((time (or time (+ 1 now))))
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
          t)))))

(declaim (inline pds-connected-p))
(defun pds-connected-p (x1 x2 time disjoint-set)
  "Checks if X1 and X2 have the same root at TIME."
  (= (pds-root x1 time disjoint-set) (pds-root x2 time disjoint-set)))

(declaim (inline pds-opening-time))
(defun pds-opening-time (x1 x2 disjoint-set)
  "Returns the earliest time when X1 and X2 were connected. Returns NIL if X1
and X2 are not connected yet."
  (labels ((bisect (ng ok)
             (declare ((integer 0 #.most-positive-fixnum) ng ok))
             (if (<= (- ok ng) 1)
                 ok
                 (let ((mid (ash (+ ng ok) -1)))
                   (if (pds-connected-p x1 x2 mid disjoint-set)
                       (bisect ng mid)
                       (bisect mid ok))))))
    (when (pds-connected-p x1 x2 (pds-now disjoint-set) disjoint-set)
      (bisect 0 (pds-now disjoint-set)))))

;; Test
(defun test-persistent-disjoint-set ()
  (let ((tree (make-persistent-disjoint-set 6)))
    (pds-unite! 1 3 tree)
    (pds-unite! 3 5 tree)
    (pds-unite! 1 5 tree)
    (pds-unite! 2 4 tree)
    (pds-unite! 1 2 tree)
    (assert (equalp (vector most-positive-fixnum most-positive-fixnum 5 1 4 2)
                    (pds-timestamps tree)))
    (assert (equalp (vector -1 -5 1 1 2 1) (pds-data tree)))))
