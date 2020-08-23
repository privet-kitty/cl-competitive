(defpackage :cp/undoable-disjoint-set
  (:use :cl)
  (:export #:make-undoable-disjoint-set #:undoable-disjoint-set
           #:uds-root #:uds-unite! #:uds-size #:uds-connected-p
           #:uds-undo! #:uds-snapshot! #:uds-rollback!))
(in-package :cp/undoable-disjoint-set)

(defstruct (undoable-disjoint-set
            (:constructor make-undoable-disjoint-set
                (size
                 &optional (buffer-size 1)
                 &aux (data (make-array size :element-type 'fixnum :initial-element -1))
                      (stack (make-array (max 4 (* 4 buffer-size)) :element-type 'fixnum))))
            (:conc-name %uds-)
            (:copier nil)
            (:predicate nil))
  (data nil :type (simple-array fixnum (*)))
  (stack nil :type (simple-array fixnum (*)))
  (end 0 :type (mod #.array-total-size-limit)))

(declaim (inline uds-root))
(defun uds-root (dset x)
  "Returns the root of the component to which X belongs."
  (let ((data (%uds-data dset)))
    (labels ((recur (x)
               (if (minusp (aref data x))
                   x
                   (recur (aref data x)))))
      (recur x))))

(defun uds-unite! (dset x1 x2)
  "Destructively unites X1 and X2. Returns true iff X1 and X2 become connected
for the first time."
  (symbol-macrolet ((stack (%uds-stack dset))
                    (end (%uds-end dset)))
    (let ((root1 (uds-root dset x1))
          (root2 (uds-root dset x2))
          (data (%uds-data dset)))
      ;; record
      (when (= end (length stack))
        (setf stack (adjust-array stack (the (integer 0 #.most-positive-fixnum)
                                             (* end 2)))))
      (setf (aref stack end) root1
            (aref stack (+ end 1)) (aref data root1)
            (aref stack (+ end 2)) root2
            (aref stack (+ end 3)) (aref data root2)
            end (+ end 4))
      (unless (= root1 root2)
        ;; Ensure the size of root1 >= the size of root2
        (when (> (aref data root1) (aref data root2))
          (rotatef root1 root2))
        ;; update
        (incf (aref data root1) (aref data root2))
        (setf (aref data root2) root1)))))

(declaim (inline uds-connected-p))
(defun uds-connected-p (dset x1 x2)
  "Returns true iff X1 and X2 have the same root."
  (= (uds-root dset x1) (uds-root dset x2)))

(declaim (inline uds-size))
(defun uds-size (dset x)
  (- (aref (%uds-data dset) (uds-root dset x))))

(defun uds-undo! (dset &optional (empty-error-p t))
  "Undoes the last UDS-UNITE!."
  (symbol-macrolet ((stack (%uds-stack dset))
                    (end (%uds-end dset))
                    (data (%uds-data dset)))
    (if (zerop end)
        (and empty-error-p (error "No undoable updates in ~W" dset))
        (setf end (- end 4)
              (aref data (aref stack end)) (aref stack (+ end 1))
              (aref data (aref stack (+ end 2))) (aref stack (+ end 3))))))

(defun uds-snapshot! (dset)
  (setf (%uds-end dset) 0))

(defun uds-rollback! (dset)
  (loop while (uds-undo! dset nil)))
