;;;
;;; Partially persistent disjoint set
;;;

;; TODO: enable to get the size of a connected component
(defstruct (persistent-disjoint-set
            (:constructor make-persistent-disjoint-set
                (size &aux
                      ;; DATA holds a negative integer as size and a
                      ;; non-negative integer as parent.
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
(defun pds-unite! (x1 x2 disjoint-set)
  "Destructively unites X1 and X2."
  (symbol-macrolet ((now (pds-now disjoint-set)))
    (incf now)
    (let ((timestamps (pds-timestamps disjoint-set))
          (data (pds-data disjoint-set))
          (root1 (pds-root x1 now disjoint-set))
          (root2 (pds-root x2 now disjoint-set)))
      (unless (= root1 root2)
        (when (> (aref data root1) (aref data root2))
          (rotatef root1 root2))
        ;; (size root1) >= (size root2)
        (incf (aref data root1) (aref data root2))
        (setf (aref data root2) root1
              (aref timestamps root2) now)
        t))))

(declaim (inline pds-connected-p))
(defun pds-connected-p (x1 x2 time disjoint-set)
  "Checks if X1 and X2 have the same root at TIME."
  (= (pds-root x1 time disjoint-set) (pds-root x2 time disjoint-set)))

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
