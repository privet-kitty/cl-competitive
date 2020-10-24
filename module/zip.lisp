(defpackage :cp/zip
  (:use :cl)
  (:import-from :sb-c #:type-specifier #:consed-sequence #:creation-result-type-specifier-nth-arg)
  (:export #:zip))
(in-package :cp/zip)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (sb-c:defknown zip (type-specifier &rest sequence) consed-sequence ()
    :derive-type (creation-result-type-specifier-nth-arg 1)
    :overwrite-fndb-silently t))

;; TODO: deftransform for fixed length of arguments
(defun zip (function sequence-type &rest seqs)
  (declare (function function))
  (let* ((len (loop for seq in seqs
                    minimize (length seq)))
         (nodes (loop for seq in seqs
                      collect (multiple-value-bind (iter limit from-end-p step endp element)
                                  (sb-sequence:make-sequence-iterator seq)
                                (declare (ignore limit from-end-p endp))
                                (list* iter step element))))
         (result (loop repeat len
                       collect (loop for node in nodes
                                     for seq in seqs
                                     for (iter step . element) = node
                                     collect (prog1 (funcall element seq iter)
                                               (setf (car node) (funcall step seq iter nil)))))))
    (coerce result sequence-type)))
