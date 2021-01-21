(defpackage :cp/binary-trie
  (:use :cl)
  (:export #:make-btrie #:btrie-insert! #:btrie-delete! #:btrie-insert #:btrie-delete
           #:btrie #:+btrie-max-depth+ #:btrie-count #:btrie-child0 #:btrie-child1)
  (:documentation "Provides (persistent) binary trie. Every fundamental
operation takes O(+BTRIE-MAX-DEPTH+) time.

Note that it is usually wrong to combinedly use destructive
operations (BTRIE-INSERT!/DELETE!) and copying
operations (bTRIE-INSERT/DELETE)."))
(in-package :cp/binary-trie)

;; NOTE: **NOT TESTED**

(defconstant +btrie-max-depth+ 32)

(declaim (inline make-btrie))
(defstruct (btrie (:constructor %make-btrie))
  (count 0 :type (integer 0 #.most-positive-fixnum))
  (child0 nil :type (or null btrie))
  (child1 nil :type (or null btrie)))

(declaim (inline make-btrie))
(defun make-btrie ()
  (%make-btrie))

(declaim (inline btrie-insert!))
(defun btrie-insert! (btrie x)
  "Destructively inserts an X to BTRIE. You can rely on the side effect."
  (let ((btrie btrie))
    (loop for pos from (- +btrie-max-depth+ 1) downto 0
          do (incf (btrie-count btrie))
             (setq btrie
                   (if (logbitp pos x)
                       (setf (btrie-child1 btrie)
                             (or (btrie-child1 btrie)
                                 (%make-btrie)))
                       (setf (btrie-child0 btrie)
                             (or (btrie-child0 btrie)
                                 (%make-btrie))))))
    (incf (btrie-count btrie)))
  btrie)

(declaim (inline btrie-insert))
(defun btrie-insert (btrie x)
  "Is a persistent version of BTRIE-INSERT! by path-copying."
  (labels ((recur (btrie pos)
             (declare ((integer -1 (#.+btrie-max-depth+)) pos))
             (let ((btrie (if btrie
                              (copy-btrie btrie)
                              (make-btrie))))
               (incf (btrie-count btrie))
               (when (>= pos 0)
                 (if (logbitp pos x)
                     (setf (btrie-child1 btrie)
                           (recur (btrie-child1 btrie) (- pos 1)))
                     (setf (btrie-child0 btrie)
                           (recur (btrie-child0 btrie) (- pos 1)))))
               btrie)))
    (recur btrie (- +btrie-max-depth+ 1))))

(declaim (inline btrie-delete!))
(defun btrie-delete! (btrie x)
  "Destructively deletes an X from BTRIE. You can rely on the side effect. The
consequence is undefined when BTRIE doesn't contain X."
  (declare (unsigned-byte x))
  (let ((btrie btrie))
    (decf (btrie-count btrie))
    (loop for pos from (- +btrie-max-depth+ 1) downto 0
          when (logbitp pos x)
          do (when (zerop (decf (btrie-count (btrie-child1 btrie))))
               (setf (btrie-child1 btrie) nil)
               (return))
             (setq btrie (btrie-child1 btrie))
          else
          do (when (zerop (decf (btrie-count (btrie-child0 btrie))))
               (setf (btrie-child0 btrie) nil)
               (return))
             (setq btrie (btrie-child0 btrie))))
  btrie)

(declaim (inline btrie-delete))
(defun btrie-delete (btrie x)
  "Is a persistent version of BTRIE-DELETE! by path-copying. The consequence is
undefined when BTRIE doesn't contain X."
  (labels ((recur (btrie pos)
             (declare ((integer -1 (#.+btrie-max-depth+)) pos))
             (when (> (btrie-count btrie) 1)
               (let ((btrie (copy-btrie btrie)))
                 (decf (btrie-count btrie))
                 (when (>= pos 0)
                   (if (logbitp pos x)
                       (setf (btrie-child1 btrie)
                             (recur (btrie-child1 btrie) (- pos 1)))
                       (setf (btrie-child0 btrie)
                             (recur (btrie-child0 btrie) (- pos 1)))))
                 btrie))))
    (or (recur btrie (- +btrie-max-depth+ 1))
        (make-btrie))))
