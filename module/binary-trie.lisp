(defpackage :cp/binary-trie
  (:use :cl)
  (:export #:make-btrie #:btrie-add! #:btrie-delete! #:btrie #:+btrie-max-depth+
           #:btrie-count #:btrie-child0 #:btrie-child1))
(in-package :cp/binary-trie)

;; NOTE: not tested

(defconstant +btrie-max-depth+ 32)

(declaim (inline make-btrie))
(defstruct (btrie (:copier nil)
                  (:constructor %make-btrie))
  (count 0 :type (integer 0 #.most-positive-fixnum))
  (child0 nil :type (or null btrie))
  (child1 nil :type (or null btrie)))

(declaim (inline make-btrie))
(defun make-btrie ()
  (%make-btrie))

(declaim (inline btrie-add!))
(defun btrie-add! (btrie x)
  "Adds an X to BTRIE."
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

(declaim (inline btrie-delete!))
(defun btrie-delete! (btrie x)
  "Deletes an X from BTRIE. The consequence is undefined when BTRIE doesn't
contain X."
  (declare (unsigned-byte x))
  (let ((btrie btrie))
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
