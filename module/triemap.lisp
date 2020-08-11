;;;
;;; TrieMap (map structure by Trie)
;;;

(defpackage :cp/triemap
  (:use :cl)
  (:export #:triemap-char-encode #:+triemap-alphabet-size+ #:make-triemap #:%make-triemap-node
           #:triemap-add! #:triemap-query #:triemap-get #:triemap-query-longest))
(in-package :cp/triemap)

;; ASCII code:
;; #\A: 65
;; #\a: 97
;; #\0: 48
(declaim (inline triemap-char-encode))
(defun triemap-char-encode (x)
  (- (char-code x) #.(char-code #\a)))

(defconstant +triemap-alphabet-size+ 26)

;; TODO: enable it to set VALUE to NIL by distinguishing null and unbound.
(declaim (inline %make-triemap-node))
(defstruct (triemap-node (:constructor %make-triemap-node
                          (&optional value
                           &aux (children (make-array #.+triemap-alphabet-size+
                                                      :element-type t
                                                      :initial-element 0))))
                         (:copier nil)
                         (:predicate nil))
  (value nil)
  (children nil :type (simple-array t (#.+triemap-alphabet-size+))))

(declaim (inline make-triemap))
(defun make-triemap () (%make-triemap-node))

(declaim (inline triemap-add!))
(defun triemap-add! (triemap-node string &optional (value t))
  "Adds STRING to the TRIEMAP-NODE and assigns VALUE to it. Note that null value
means the string doesn't exist in the triemap: that is, (triemap-add!
<triemap-node> <string> nil) virtually works as a deletion of <string>."
  (declare (vector string))
  (let ((end (length string)))
    (labels ((recur (node position)
               (if (= position end)
                   (unless (triemap-node-value node)
                     (setf (triemap-node-value node) value))
                   (let ((children (triemap-node-children node))
                         (char (triemap-char-encode (aref string position))))
                     (when (eql 0 (aref children char))
                       (setf (aref children char) (%make-triemap-node)))
                     (recur (aref children char) (+ 1 position))))))
      (recur triemap-node 0)
      triemap-node)))

(declaim (inline triemap-query))
(defun triemap-query (triemap-node string function &key (start 0) end)
  "Calls FUNCTION for each prefix of STRING existing in TRIEMAP-NODE. FUNCTION
takes two arguments: the end position and the assigned value."
  (declare (vector string)
           ((integer 0 #.most-positive-fixnum) start)
           ((or null (integer 0 #.most-positive-fixnum)) end)
           (function function))
  (let ((end (or end (length string))))
    (labels ((recur (node position)
               (when (triemap-node-value node)
                 (funcall function position (triemap-node-value node)))
               (unless (= position end)
                 (let ((children (triemap-node-children node))
                       (char (triemap-char-encode (aref string position))))
                   (unless (eql 0 (aref children char))
                     (recur (aref children char) (+ 1 position)))))))
      (recur triemap-node start))))

(defun triemap-get (triemap-node string &key (start 0) end)
  "Finds STRING in TRIEMAP-NODE and returns the assigned value if it exists,
otherwise NIL."
  (declare (vector string)
           ((integer 0 #.most-positive-fixnum) start)
           ((or null (integer 0 #.most-positive-fixnum)) end))
  (let ((end (or end (length string))))
    (labels ((recur (node position)
               (if (= position end)
                   (triemap-node-value node)
                   (let ((children (triemap-node-children node))
                         (char (triemap-char-encode (aref string position))))
                     (unless (eql 0 (aref children char))
                       (recur (aref children char) (+ 1 position)))))))
      (recur triemap-node start))))

(defun triemap-query-longest (triemap-node string &key (start 0) end)
  "Returns the end position and the value of the longest word in TRIEMAP-NODE which
coincides with a prefix of STRING. Returns NIL when no such words exist."
  (declare (vector string)
           ((integer 0 #.most-positive-fixnum) start)
           ((or null (integer 0 #.most-positive-fixnum)) end))
  (let ((end (or end (length string)))
        result-position
        result-value)
    (declare ((or null (integer 0 #.most-positive-fixnum)) result-position))
    (labels ((recur (node position)
               (when (triemap-node-value node)
                 (setq result-position position
                       result-value (triemap-node-value node)))
               (unless (= position end)
                 (let ((children (triemap-node-children node))
                       (char (triemap-char-encode (aref string position))))
                   (unless (eql 0 (aref children char))
                     (recur (aref children char) (+ 1 position)))))))
      (recur triemap-node start)
      (values result-position result-value))))

