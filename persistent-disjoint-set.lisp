(deftype non-negative-fixnum () '(integer 0 #.most-positive-fixnum))

(defstruct (persistent-union-find
            (:constructor make-persistent-union-find
                (size &aux (parents (let ((seq (make-array size :element-type 'non-negative-fixnum)))
                                      (dotimes (i size seq) (setf (aref seq i) i))))
                           (ranks (make-array size :element-type 'non-negative-fixnum
                                                   :initial-element 0))
                           (timestamps (make-array size :element-type 'non-negative-fixnum
                                                       :initial-element most-positive-fixnum))))
            (:conc-name puf-))
  "partially persistent Union-Find"
  (parents nil :type (simple-array non-negative-fixnum (*)))
  (ranks nil :type (simple-array non-negative-fixnum (*)))
  (now 0 :type non-negative-fixnum)
  (timestamps nil :type (simple-array non-negative-fixnum (*))))

(declaim (ftype (function * (values non-negative-fixnum &optional)) puf-root))
(defun puf-root (x time puf-tree)
  "Returns the root of X at TIME."
  (declare (optimize (speed 3))
           (non-negative-fixnum x time))
  (if (< time (aref (puf-timestamps puf-tree) x))
      x
      (puf-root (aref (puf-parents puf-tree) x) time puf-tree)))

(declaim (inline puf-unite!))
(defun puf-unite! (x1 x2 puf-tree)
  "Destructively unites X1 and X2."
  (symbol-macrolet ((now (puf-now puf-tree))
                    (timestamps (puf-timestamps puf-tree))
                    (parents (puf-parents puf-tree))
                    (ranks (puf-ranks puf-tree)))
    (incf now)
    (let ((root1 (puf-root x1 now puf-tree))
          (root2 (puf-root x2 now puf-tree)))
      (unless (= root1 root2)
        (when (< (aref ranks root1) (aref ranks root2))
          (rotatef root1 root2))
        ;; (rank root1) >= (rank root2)
        (setf (aref parents root2) root1
              (aref timestamps root2) now)
        (when (= (aref ranks root1) (aref ranks root2))
          (incf (aref ranks root1)))))))

(declaim (inline puf-connected-p))
(defun puf-connected-p (x1 x2 time puf-tree)
  "Checks if X1 and X2 have the same root at TIME."
  (= (puf-root x1 time puf-tree) (puf-root x2 time puf-tree)))

;; Test
;; (let ((tree (make-persistent-union-find 6)))
;;   (puf-unite! 1 3 tree)
;;   (puf-unite! 3 5 tree)
;;   (puf-unite! 1 5 tree)
;;   (puf-unite! 2 4 tree)
;;   (puf-unite! 1 2 tree)
;;   (assert (equalp (vector most-positive-fixnum most-positive-fixnum 5 1 4 2)
;;                   (puf-timestamps tree)))
;;   (assert (equalp (vector 0 1 1 1 2 1) (puf-parents tree))))
