(defpackage :cp/triemap
  (:use :cl)
  (:export #:triemap-char-encode #:+triemap-alphabet-size+ #:+null-triemap+
           #:make-triemap #:%make-triemap
           #:triemap-insert! #:triemap-query #:triemap-get #:triemap-query-longest)
  (:documentation "Provides map structure by trie."))
(in-package :cp/triemap)

;; ASCII code:
;; #\A: 65
;; #\a: 97
;; #\0: 48
(declaim (inline triemap-char-encode))
(defun triemap-char-encode (x)
  (- (char-code x) #.(char-code #\a)))

(defconstant +triemap-alphabet-size+ 26)
(defconstant +null-triemap+ 0)

;; TODO: enable it to set VALUE to NIL by distinguishing null and unbound.
(declaim (inline %make-triemap))
(defstruct (triemap (:constructor %make-triemap
                        (&optional value
                         &aux (children (make-array +triemap-alphabet-size+
                                                    :element-type t
                                                    :initial-element +null-triemap+))))
                    (:copier nil)
                    (:predicate nil))
  (value nil)
  (children nil :type (simple-array t (#.+triemap-alphabet-size+))))

(declaim (inline make-triemap))
(defun make-triemap () (%make-triemap))

(declaim (inline triemap-insert!))
(defun triemap-insert! (triemap string &optional (value t))
  "Inserts STRING to the TRIEMAP and assigns VALUE to it. Note that null value
means the string doesn't exist in the triemap: that is, (triemap-insert!
<triemap> <string> nil) virtually works as a deletion of <string>."
  (declare (vector string))
  (let ((end (length string)))
    (labels ((recur (node position)
               (if (= position end)
                   (unless (triemap-value node)
                     (setf (triemap-value node) value))
                   (let ((children (triemap-children node))
                         (char (triemap-char-encode (aref string position))))
                     (when (eql +null-triemap+ (aref children char))
                       (setf (aref children char) (%make-triemap)))
                     (recur (aref children char) (+ 1 position))))))
      (recur triemap 0)
      triemap)))

(declaim (inline triemap-query))
(defun triemap-query (triemap string function &key (start 0) end)
  "Calls FUNCTION for each prefix of STRING existing in TRIEMAP. FUNCTION
takes two arguments: the end position and the assigned value."
  (declare (vector string)
           ((mod #.array-dimension-limit) start)
           ((or null (mod #.array-dimension-limit)) end)
           (function function))
  (let ((end (or end (length string))))
    (labels ((recur (node position)
               (when (triemap-value node)
                 (funcall function position (triemap-value node)))
               (unless (= position end)
                 (let ((children (triemap-children node))
                       (char (triemap-char-encode (aref string position))))
                   (unless (eql +null-triemap+ (aref children char))
                     (recur (aref children char) (+ 1 position)))))))
      (recur triemap start))))

(defun triemap-get (triemap string &key (start 0) end)
  "Finds STRING in TRIEMAP and returns the assigned value if it exists,
otherwise NIL."
  (declare (vector string)
           ((mod #.array-dimension-limit) start)
           ((or null (mod #.array-dimension-limit)) end))
  (let ((end (or end (length string))))
    (labels ((recur (node position)
               (if (= position end)
                   (triemap-value node)
                   (let ((children (triemap-children node))
                         (char (triemap-char-encode (aref string position))))
                     (unless (eql +null-triemap+ (aref children char))
                       (recur (aref children char) (+ 1 position)))))))
      (recur triemap start))))

(defun triemap-query-longest (triemap string &key (start 0) end)
  "Returns the end position and the value of the longest word in TRIEMAP which
coincides with a prefix of STRING. Returns NIL when no such words exist."
  (declare (vector string)
           ((mod #.array-dimension-limit) start)
           ((or null (mod #.array-dimension-limit)) end))
  (let ((end (or end (length string)))
        result-position
        result-value)
    (declare ((or null (mod #.array-dimension-limit)) result-position))
    (labels ((recur (node position)
               (when (triemap-value node)
                 (setq result-position position
                       result-value (triemap-value node)))
               (unless (= position end)
                 (let ((children (triemap-children node))
                       (char (triemap-char-encode (aref string position))))
                   (unless (eql +null-triemap+ (aref children char))
                     (recur (aref children char) (+ 1 position)))))))
      (recur triemap start)
      (values result-position result-value))))

